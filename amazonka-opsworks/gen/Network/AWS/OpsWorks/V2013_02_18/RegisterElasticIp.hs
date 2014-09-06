{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.RegisterElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers an Elastic IP address with a specified stack. An address can be
-- registered with only one stack at a time. If the address is already
-- registered, you must first deregister it by calling DeregisterElasticIp.
-- For more information, see Resource Management. Required Permissions: To use
-- this action, an IAM user must have a Manage permissions level for the
-- stack, or an attached policy that explicitly grants permissions. For more
-- information on user permissions, see Managing User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.RegisterElasticIp
    (
    -- * Request
      RegisterElasticIp
    -- ** Request constructor
    , mkRegisterElasticIp
    -- ** Request lenses
    , reiElasticIp
    , reiStackId

    -- * Response
    , RegisterElasticIpResponse
    -- ** Response lenses
    , reirsElasticIp
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data RegisterElasticIp = RegisterElasticIp
    { _reiElasticIp :: Text
    , _reiStackId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterElasticIp' request.
mkRegisterElasticIp :: Text -- ^ 'reiElasticIp'
                    -> Text -- ^ 'reiStackId'
                    -> RegisterElasticIp
mkRegisterElasticIp p1 p2 = RegisterElasticIp
    { _reiElasticIp = p1
    , _reiStackId = p2
    }
{-# INLINE mkRegisterElasticIp #-}

-- | The Elastic IP address.
reiElasticIp :: Lens' RegisterElasticIp Text
reiElasticIp = lens _reiElasticIp (\s a -> s { _reiElasticIp = a })
{-# INLINE reiElasticIp #-}

-- | The stack ID.
reiStackId :: Lens' RegisterElasticIp Text
reiStackId = lens _reiStackId (\s a -> s { _reiStackId = a })
{-# INLINE reiStackId #-}

instance ToPath RegisterElasticIp

instance ToQuery RegisterElasticIp

instance ToHeaders RegisterElasticIp

instance ToJSON RegisterElasticIp

-- | Contains the response to a RegisterElasticIp request.
newtype RegisterElasticIpResponse = RegisterElasticIpResponse
    { _reirsElasticIp :: Maybe Text
    } deriving (Show, Generic)

-- | The Elastic IP address.
reirsElasticIp :: Lens' RegisterElasticIpResponse (Maybe Text)
reirsElasticIp = lens _reirsElasticIp (\s a -> s { _reirsElasticIp = a })
{-# INLINE reirsElasticIp #-}

instance FromJSON RegisterElasticIpResponse

instance AWSRequest RegisterElasticIp where
    type Sv RegisterElasticIp = OpsWorks
    type Rs RegisterElasticIp = RegisterElasticIpResponse

    request = get
    response _ = jsonResponse
