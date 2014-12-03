{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Network.AWS.Internal.Env
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.Env where

import Control.Lens
import Data.List                (intersperse)
import Data.Monoid
import Network.AWS.Data         (ToBuilder(..))
import Network.AWS.Internal.Log
import Network.AWS.Types        (Region, Auth)
import Network.HTTP.Conduit

-- | The environment containing the parameters required to make AWS requests.
data Env = Env
    { _envRegion  :: !Region
    , _envLogger  :: Logger
    , _envManager :: Manager
    , _envRetry   :: !Bool
    , _envAuth    :: Auth
    }

makeLenses ''Env

instance ToBuilder Env where
    build Env{..} = mconcat $ intersperse "\n"
        [ "[Environment] {"
        , "  region = " <> build _envRegion
        , "  auth   = " <> build _envAuth
        , "  retry  = " <> build _envRetry
        , "}"
        ]
