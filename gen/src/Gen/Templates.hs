{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.Templates
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Templates (loadTemplates) where

import Control.Applicative
import Control.Error
import Gen.IO
import Gen.Types
import System.FilePath
import Text.EDE            (eitherParseFile)

loadTemplates :: FilePath -> Script Templates
loadTemplates d = do
    f  <- Templates
        <$> load "cabal"
        <*> load "service"
        <*> load "readme"
        <*> load "example-cabal"
        <*> load "example-makefile"

    !x <- (,)
        <$> load "types-xml"
        <*> load "operation-xml"

    !j <- (,)
        <$> load "types-json"
        <*> load "operation-json"

    !q <- (,)
        <$> load "types-query"
        <*> load "operation-query"

    return $! f $ \case
        Json     -> j
        RestJson -> j
        Xml      -> x
        RestXml  -> x
        Query    -> q
        Ec2      -> q
  where
    load (path -> f) =
           say "Parse Template" f
        *> scriptIO (eitherParseFile f)
       >>= hoistEither

    path f = d </> f <.> "ede"
