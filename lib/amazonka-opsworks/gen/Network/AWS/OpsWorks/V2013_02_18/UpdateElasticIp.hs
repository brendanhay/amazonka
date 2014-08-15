{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.UpdateElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a registered Elastic IP address's name. For more information, see
-- Resource Management. Required Permissions: To use this action, an IAM user
-- must have a Manage permissions level for the stack, or an attached policy
-- that explicitly grants permissions. For more information on user
-- permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.UpdateElasticIp where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'UpdateElasticIp' request.
updateElasticIp :: Text -- ^ '_ueirElasticIp'
                -> UpdateElasticIp
updateElasticIp p1 = UpdateElasticIp
    { _ueirElasticIp = p1
    , _ueirName = Nothing
    }

data UpdateElasticIp = UpdateElasticIp
    { _ueirElasticIp :: Text
      -- ^ The address.
    , _ueirName :: Maybe Text
      -- ^ The new name.
    } deriving (Show, Generic)

makeLenses ''UpdateElasticIp

instance ToPath UpdateElasticIp

instance ToQuery UpdateElasticIp

instance ToHeaders UpdateElasticIp

instance ToJSON UpdateElasticIp

data UpdateElasticIpResponse = UpdateElasticIpResponse
    deriving (Eq, Show, Generic)

makeLenses ''UpdateElasticIpResponse

instance AWSRequest UpdateElasticIp where
    type Sv UpdateElasticIp = OpsWorks
    type Rs UpdateElasticIp = UpdateElasticIpResponse

    request = get
    response _ = nullaryResponse UpdateElasticIpResponse
