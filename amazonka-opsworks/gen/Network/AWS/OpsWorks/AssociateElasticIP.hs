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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one of the stack's registered Elastic IP addresses with a specified instance. The address must first be registered with the stack by calling 'RegisterElasticIp' . For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.AssociateElasticIP
    (
    -- * Creating a Request
      associateElasticIP
    , AssociateElasticIP
    -- * Request Lenses
    , aeiInstanceId
    , aeiElasticIP

    -- * Destructuring the Response
    , associateElasticIPResponse
    , AssociateElasticIPResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateElasticIP' smart constructor.
data AssociateElasticIP = AssociateElasticIP'
  { _aeiInstanceId :: !(Maybe Text)
  , _aeiElasticIP  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateElasticIP' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aeiInstanceId' - The instance ID.
--
-- * 'aeiElasticIP' - The Elastic IP address.
associateElasticIP
    :: Text -- ^ 'aeiElasticIP'
    -> AssociateElasticIP
associateElasticIP pElasticIP_ =
  AssociateElasticIP' {_aeiInstanceId = Nothing, _aeiElasticIP = pElasticIP_}


-- | The instance ID.
aeiInstanceId :: Lens' AssociateElasticIP (Maybe Text)
aeiInstanceId = lens _aeiInstanceId (\ s a -> s{_aeiInstanceId = a})

-- | The Elastic IP address.
aeiElasticIP :: Lens' AssociateElasticIP Text
aeiElasticIP = lens _aeiElasticIP (\ s a -> s{_aeiElasticIP = a})

instance AWSRequest AssociateElasticIP where
        type Rs AssociateElasticIP =
             AssociateElasticIPResponse
        request = postJSON opsWorks
        response = receiveNull AssociateElasticIPResponse'

instance Hashable AssociateElasticIP where

instance NFData AssociateElasticIP where

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
              (catMaybes
                 [("InstanceId" .=) <$> _aeiInstanceId,
                  Just ("ElasticIp" .= _aeiElasticIP)])

instance ToPath AssociateElasticIP where
        toPath = const "/"

instance ToQuery AssociateElasticIP where
        toQuery = const mempty

-- | /See:/ 'associateElasticIPResponse' smart constructor.
data AssociateElasticIPResponse =
  AssociateElasticIPResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateElasticIPResponse' with the minimum fields required to make a request.
--
associateElasticIPResponse
    :: AssociateElasticIPResponse
associateElasticIPResponse = AssociateElasticIPResponse'


instance NFData AssociateElasticIPResponse where
