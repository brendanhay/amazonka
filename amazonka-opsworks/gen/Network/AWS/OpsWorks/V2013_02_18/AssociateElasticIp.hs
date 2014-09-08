{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.V2013_02_18.AssociateElasticIp
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Associates one of the stack's registered Elastic IP addresses with a
-- specified instance. The address must first be registered with the stack by
-- calling RegisterElasticIp. For more information, see Resource Management.
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly
-- grants permissions. For more information on user permissions, see Managing
-- User Permissions.
module Network.AWS.OpsWorks.V2013_02_18.AssociateElasticIp
    (
    -- * Request
      AssociateElasticIp
    -- ** Request constructor
    , mkAssociateElasticIp
    -- ** Request lenses
    , aeiElasticIp
    , aeiInstanceId

    -- * Response
    , AssociateElasticIpResponse
    -- ** Response constructor
    , mkAssociateElasticIpResponse
    ) where

import Network.AWS.OpsWorks.V2013_02_18.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data AssociateElasticIp = AssociateElasticIp
    { _aeiElasticIp :: Text
    , _aeiInstanceId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssociateElasticIp' request.
mkAssociateElasticIp :: Text -- ^ 'aeiElasticIp'
                     -> AssociateElasticIp
mkAssociateElasticIp p1 = AssociateElasticIp
    { _aeiElasticIp = p1
    , _aeiInstanceId = Nothing
    }

-- | The Elastic IP address.
aeiElasticIp :: Lens' AssociateElasticIp Text
aeiElasticIp = lens _aeiElasticIp (\s a -> s { _aeiElasticIp = a })

-- | The instance ID.
aeiInstanceId :: Lens' AssociateElasticIp (Maybe Text)
aeiInstanceId = lens _aeiInstanceId (\s a -> s { _aeiInstanceId = a })

instance ToPath AssociateElasticIp

instance ToQuery AssociateElasticIp

instance ToHeaders AssociateElasticIp

instance ToJSON AssociateElasticIp

data AssociateElasticIpResponse = AssociateElasticIpResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'AssociateElasticIpResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkAssociateElasticIpResponse :: AssociateElasticIpResponse
mkAssociateElasticIpResponse = AssociateElasticIpResponse

instance AWSRequest AssociateElasticIp where
    type Sv AssociateElasticIp = OpsWorks
    type Rs AssociateElasticIp = AssociateElasticIpResponse

    request = get
    response _ = nullaryResponse AssociateElasticIpResponse
