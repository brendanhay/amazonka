{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.AssociateElasticIp
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
-- calling 'RegisterElasticIp'. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- Required Permissions: To use this action, an IAM user must have a Manage
-- permissions level for the stack, or an attached policy that explicitly grants
-- permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing UserPermissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_AssociateElasticIp.html>
module Network.AWS.OpsWorks.AssociateElasticIp
    (
    -- * Request
      AssociateElasticIp
    -- ** Request constructor
    , associateElasticIp
    -- ** Request lenses
    , aeiElasticIp
    , aeiInstanceId

    -- * Response
    , AssociateElasticIpResponse
    -- ** Response constructor
    , associateElasticIpResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data AssociateElasticIp = AssociateElasticIp
    { _aeiElasticIp  :: Text
    , _aeiInstanceId :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'AssociateElasticIp' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aeiElasticIp' @::@ 'Text'
--
-- * 'aeiInstanceId' @::@ 'Maybe' 'Text'
--
associateElasticIp :: Text -- ^ 'aeiElasticIp'
                   -> AssociateElasticIp
associateElasticIp p1 = AssociateElasticIp
    { _aeiElasticIp  = p1
    , _aeiInstanceId = Nothing
    }

-- | The Elastic IP address.
aeiElasticIp :: Lens' AssociateElasticIp Text
aeiElasticIp = lens _aeiElasticIp (\s a -> s { _aeiElasticIp = a })

-- | The instance ID.
aeiInstanceId :: Lens' AssociateElasticIp (Maybe Text)
aeiInstanceId = lens _aeiInstanceId (\s a -> s { _aeiInstanceId = a })

data AssociateElasticIpResponse = AssociateElasticIpResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'AssociateElasticIpResponse' constructor.
associateElasticIpResponse :: AssociateElasticIpResponse
associateElasticIpResponse = AssociateElasticIpResponse

instance ToPath AssociateElasticIp where
    toPath = const "/"

instance ToQuery AssociateElasticIp where
    toQuery = const mempty

instance ToHeaders AssociateElasticIp

instance ToJSON AssociateElasticIp where
    toJSON AssociateElasticIp{..} = object
        [ "ElasticIp"  .= _aeiElasticIp
        , "InstanceId" .= _aeiInstanceId
        ]

instance AWSRequest AssociateElasticIp where
    type Sv AssociateElasticIp = OpsWorks
    type Rs AssociateElasticIp = AssociateElasticIpResponse

    request  = post "AssociateElasticIp"
    response = nullResponse AssociateElasticIpResponse
