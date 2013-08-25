{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Test.TH
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.TH
    (
    -- * Specific Derivations
      deriveDependency
    , deriveProperty

    -- * Individual Derivations
    , deriveJSON
    , deriveArbitrary
    , deriveTemplate
    ) where

import           Control.Monad
import qualified Data.Aeson.TH         as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.DeriveTH         as Derive
import           Data.List
import           Language.Haskell.TH
import           Network.AWS.Internal
import           System.Directory

deriveDependency :: [Name] -> Q [Dec]
deriveDependency names = liftM concat $ mapM ($ names)
     [ deriveJSON
     , deriveArbitrary
     ]

deriveProperty :: FilePath -> [Name] -> Q [Dec]
deriveProperty dir names = liftM concat $ mapM ($ names)
     [ deriveJSON
     , deriveArbitrary
     , deriveTemplate dir
     ]

deriveJSON :: [Name] -> Q [Dec]
deriveJSON = liftM concat . mapM (Aeson.deriveToJSON id)

deriveArbitrary :: [Name] -> Q [Dec]
deriveArbitrary = Derive.derives [Derive.makeArbitrary]

deriveTemplate :: FilePath -> [Name] -> Q [Dec]
deriveTemplate dir = liftM concat . mapM derive
  where
    derive name =
        [d|instance Template $(conT name) where
              readTemplate _ = $(template >>= embed)|]
      where
        template = runIO $ fullPath `fmap` getCurrentDirectory >>= BS.readFile

        fullPath = (++ concat ["/", dropPrefix "/" dir, "/", nameBase name])

        embed bstr = do
            pack <- [| BS.pack |]
            return $! AppE pack $! LitE $! StringL $! BS.unpack bstr
