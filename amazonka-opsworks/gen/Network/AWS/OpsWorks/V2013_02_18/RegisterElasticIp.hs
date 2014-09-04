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
    , mkRegisterElasticIpRequest
    -- ** Request lenses
    , reirElasticIp
    , reirStackId

    -- * Response
    , RegisterElasticIpResponse
    -- ** Response lenses
    , reisElasticIp
    ) where

import           Network.AWS.OpsWorks.V2013_02_18.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RegisterElasticIp' request.
mkRegisterElasticIpRequest :: Text -- ^ 'reirElasticIp'
                           -> Text -- ^ 'reirStackId'
                           -> RegisterElasticIp
mkRegisterElasticIpRequest p1 p2 = RegisterElasticIp
    { _reirElasticIp = p1
    , _reirStackId = p2
    }
{-# INLINE mkRegisterElasticIpRequest #-}

data RegisterElasticIp = RegisterElasticIp
    { _reirElasticIp :: Text
      -- ^ The Elastic IP address.
    , _reirStackId :: Text
      -- ^ The stack ID.
    } deriving (Show, Generic)

-- | The Elastic IP address.
reirElasticIp :: Lens' RegisterElasticIp (Text)
reirElasticIp = lens _reirElasticIp (\s a -> s { _reirElasticIp = a })
{-# INLINE reirElasticIp #-}

-- | The stack ID.
reirStackId :: Lens' RegisterElasticIp (Text)
reirStackId = lens _reirStackId (\s a -> s { _reirStackId = a })
{-# INLINE reirStackId #-}

instance ToPath RegisterElasticIp

instance ToQuery RegisterElasticIp

instance ToHeaders RegisterElasticIp

instance ToJSON RegisterElasticIp

newtype RegisterElasticIpResponse = RegisterElasticIpResponse
    { _reisElasticIp :: Maybe Text
      -- ^ The Elastic IP address.
    } deriving (Show, Generic)

-- | The Elastic IP address.
reisElasticIp :: Lens' RegisterElasticIpResponse (Maybe Text)
reisElasticIp = lens _reisElasticIp (\s a -> s { _reisElasticIp = a })
{-# INLINE reisElasticIp #-}

instance FromJSON RegisterElasticIpResponse

instance AWSRequest RegisterElasticIp where
    type Sv RegisterElasticIp = OpsWorks
    type Rs RegisterElasticIp = RegisterElasticIpResponse

    request = get
    response _ = jsonResponse
