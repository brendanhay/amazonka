{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes     #-}

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

module Network.AWS.TH where

import qualified Data.ByteString.Char8 as BS
import           Language.Haskell.TH
import           Paths_haws            (getDataFileName)

embedTemplate :: FilePath -> Q Exp
embedTemplate name = (runIO $ getDataFileName name >>= BS.readFile) >>= bsExp
  where
    bsExp bs = do
        pack <- [| BS.pack |]
        return $! AppE pack $! LitE $! StringL $! BS.unpack bs
