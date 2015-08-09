{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.AssociateElasticIP
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one of the stack\'s registered Elastic IP addresses with a
-- specified instance. The address must first be registered with the stack
-- by calling RegisterElasticIp. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_AssociateElasticIP.html AWS API Reference> for AssociateElasticIP.
module Network.AWS.OpsWorks.AssociateElasticIP
    (
    -- * Creating a Request
      AssociateElasticIP
    , associateElasticIP
    -- * Request Lenses
    , aeiInstanceId
    , aeiElasticIP

    -- * Destructuring the Response
    , AssociateElasticIPResponse
    , associateElasticIPResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'associateElasticIP' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aeiInstanceId'
--
-- * 'aeiElasticIP'
data AssociateElasticIP = AssociateElasticIP'
    { _aeiInstanceId :: !(Maybe Text)
    , _aeiElasticIP  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociateElasticIP' smart constructor.
associateElasticIP :: Text -> AssociateElasticIP
associateElasticIP pElasticIP_ =
    AssociateElasticIP'
    { _aeiInstanceId = Nothing
    , _aeiElasticIP = pElasticIP_
    }

-- | The instance ID.
aeiInstanceId :: Lens' AssociateElasticIP (Maybe Text)
aeiInstanceId = lens _aeiInstanceId (\ s a -> s{_aeiInstanceId = a});

-- | The Elastic IP address.
aeiElasticIP :: Lens' AssociateElasticIP Text
aeiElasticIP = lens _aeiElasticIP (\ s a -> s{_aeiElasticIP = a});

instance AWSRequest AssociateElasticIP where
        type Sv AssociateElasticIP = OpsWorks
        type Rs AssociateElasticIP =
             AssociateElasticIPResponse
        request = postJSON
        response = receiveNull AssociateElasticIPResponse'

instance ToHeaders AssociateElasticIP where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.AssociateElasticIp" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateElasticIP where
        toJSON AssociateElasticIP'{..}
          = object
              ["InstanceId" .= _aeiInstanceId,
               "ElasticIp" .= _aeiElasticIP]

instance ToPath AssociateElasticIP where
        toPath = const "/"

instance ToQuery AssociateElasticIP where
        toQuery = const mempty

-- | /See:/ 'associateElasticIPResponse' smart constructor.
data AssociateElasticIPResponse =
    AssociateElasticIPResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'AssociateElasticIPResponse' smart constructor.
associateElasticIPResponse :: AssociateElasticIPResponse
associateElasticIPResponse = AssociateElasticIPResponse'
