{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module      : Network.AWS.TH
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.TH
    ( deriveTemplate
    ) where

import           Control.Applicative
import           Data.Aeson.TH
import qualified Data.ByteString.Char8 as BS
import           Data.Char             (isUpper, toLower)
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Language.Haskell.TH
import           Network.AWS.Types
import           Paths_haws            (getDataFileName)

deriveTemplate :: String -> Name -> Q [Dec]
deriveTemplate pre name = concat <$> sequence
    [ deriveToJSON key name
    , embedTemplate name
    ]
  where
    key s = underscore . fromMaybe s $ pre `stripPrefix` s

--
-- Internal
--

-- Tries to read: template/<NameOfModule>/<Type>
-- IE: Network.AWS.Route53.CreateHealthCheck
-- becomes template/Route53/CreateHealthCheck
embedTemplate :: Name -> Q [Dec]
embedTemplate name = [d|
    instance AWSTemplate $(conT name) where
        readTemplate _ = $(readTemplate' (suffix $ show name))
    |]

readTemplate' :: FilePath -> Q Exp
readTemplate' name =
    (runIO $ getDataFileName ("template/" <> name) >>= BS.readFile) >>= bsExp
  where
    bsExp bs = do
        pack <- [| BS.pack |]
        return $! AppE pack $! LitE $! StringL $! BS.unpack bs

suffix :: String -> String
suffix str = map rep $ drop idx str
  where
    idx = (+ 1) $ reverse (elemIndices '.' str) !! 1

    rep '.' = '/'
    rep  c  = c

underscore :: String -> String
underscore (x:xs) | isUpper x = toLower x : underscore xs
underscore xs                 = concatMap f xs
  where
    f x = ['_' | isUpper x] ++ [toLower x]
