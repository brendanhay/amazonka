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
-- Module      : Network.AWS.CloudHSM.CreateHSM
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an uninitialized HSM instance. Running this command provisions
-- an HSM appliance and will result in charges to your AWS account for the
-- HSM.
--
-- /See:/ <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_CreateHSM.html AWS API Reference> for CreateHSM.
module Network.AWS.CloudHSM.CreateHSM
    (
    -- * Creating a Request
      createHSM
    , CreateHSM
    -- * Request Lenses
    , chClientToken
    , chSyslogIP
    , chExternalId
    , chEniIP
    , chSubnetId
    , chSSHKey
    , chIAMRoleARN
    , chSubscriptionType

    -- * Destructuring the Response
    , createHSMResponse
    , CreateHSMResponse
    -- * Response Lenses
    , chsmrsHSMARN
    , chsmrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.CloudHSM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the CreateHsm action.
--
-- /See:/ 'createHSM' smart constructor.
data CreateHSM = CreateHSM'
    { _chClientToken      :: !(Maybe Text)
    , _chSyslogIP         :: !(Maybe Text)
    , _chExternalId       :: !(Maybe Text)
    , _chEniIP            :: !(Maybe Text)
    , _chSubnetId         :: !Text
    , _chSSHKey           :: !Text
    , _chIAMRoleARN       :: !Text
    , _chSubscriptionType :: !SubscriptionType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateHSM' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chClientToken'
--
-- * 'chSyslogIP'
--
-- * 'chExternalId'
--
-- * 'chEniIP'
--
-- * 'chSubnetId'
--
-- * 'chSSHKey'
--
-- * 'chIAMRoleARN'
--
-- * 'chSubscriptionType'
createHSM
    :: Text -- ^ 'chSubnetId'
    -> Text -- ^ 'chSSHKey'
    -> Text -- ^ 'chIAMRoleARN'
    -> SubscriptionType -- ^ 'chSubscriptionType'
    -> CreateHSM
createHSM pSubnetId_ pSSHKey_ pIAMRoleARN_ pSubscriptionType_ =
    CreateHSM'
    { _chClientToken = Nothing
    , _chSyslogIP = Nothing
    , _chExternalId = Nothing
    , _chEniIP = Nothing
    , _chSubnetId = pSubnetId_
    , _chSSHKey = pSSHKey_
    , _chIAMRoleARN = pIAMRoleARN_
    , _chSubscriptionType = pSubscriptionType_
    }

-- | A user-defined token to ensure idempotence. Subsequent calls to this
-- action with the same token will be ignored.
chClientToken :: Lens' CreateHSM (Maybe Text)
chClientToken = lens _chClientToken (\ s a -> s{_chClientToken = a});

-- | The IP address for the syslog monitoring server.
chSyslogIP :: Lens' CreateHSM (Maybe Text)
chSyslogIP = lens _chSyslogIP (\ s a -> s{_chSyslogIP = a});

-- | The external ID from __IamRoleArn__, if present.
chExternalId :: Lens' CreateHSM (Maybe Text)
chExternalId = lens _chExternalId (\ s a -> s{_chExternalId = a});

-- | The IP address to assign to the HSM\'s ENI.
chEniIP :: Lens' CreateHSM (Maybe Text)
chEniIP = lens _chEniIP (\ s a -> s{_chEniIP = a});

-- | The identifier of the subnet in your VPC in which to place the HSM.
chSubnetId :: Lens' CreateHSM Text
chSubnetId = lens _chSubnetId (\ s a -> s{_chSubnetId = a});

-- | The SSH public key to install on the HSM.
chSSHKey :: Lens' CreateHSM Text
chSSHKey = lens _chSSHKey (\ s a -> s{_chSSHKey = a});

-- | The ARN of an IAM role to enable the AWS CloudHSM service to allocate an
-- ENI on your behalf.
chIAMRoleARN :: Lens' CreateHSM Text
chIAMRoleARN = lens _chIAMRoleARN (\ s a -> s{_chIAMRoleARN = a});

-- | The subscription type.
chSubscriptionType :: Lens' CreateHSM SubscriptionType
chSubscriptionType = lens _chSubscriptionType (\ s a -> s{_chSubscriptionType = a});

instance AWSRequest CreateHSM where
        type Sv CreateHSM = CloudHSM
        type Rs CreateHSM = CreateHSMResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateHSMResponse' <$>
                   (x .?> "HsmArn") <*> (pure (fromEnum s)))

instance ToHeaders CreateHSM where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CloudHsmFrontendService.CreateHsm" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHSM where
        toJSON CreateHSM'{..}
          = object
              (catMaybes
                 [("ClientToken" .=) <$> _chClientToken,
                  ("SyslogIp" .=) <$> _chSyslogIP,
                  ("ExternalId" .=) <$> _chExternalId,
                  ("EniIp" .=) <$> _chEniIP,
                  Just ("SubnetId" .= _chSubnetId),
                  Just ("SshKey" .= _chSSHKey),
                  Just ("IamRoleArn" .= _chIAMRoleARN),
                  Just ("SubscriptionType" .= _chSubscriptionType)])

instance ToPath CreateHSM where
        toPath = const "/"

instance ToQuery CreateHSM where
        toQuery = const mempty

-- | Contains the output of the CreateHsm action.
--
-- /See:/ 'createHSMResponse' smart constructor.
data CreateHSMResponse = CreateHSMResponse'
    { _chsmrsHSMARN :: !(Maybe Text)
    , _chsmrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateHSMResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chsmrsHSMARN'
--
-- * 'chsmrsStatus'
createHSMResponse
    :: Int -- ^ 'chsmrsStatus'
    -> CreateHSMResponse
createHSMResponse pStatus_ =
    CreateHSMResponse'
    { _chsmrsHSMARN = Nothing
    , _chsmrsStatus = pStatus_
    }

-- | The ARN of the HSM.
chsmrsHSMARN :: Lens' CreateHSMResponse (Maybe Text)
chsmrsHSMARN = lens _chsmrsHSMARN (\ s a -> s{_chsmrsHSMARN = a});

-- | The response status code.
chsmrsStatus :: Lens' CreateHSMResponse Int
chsmrsStatus = lens _chsmrsStatus (\ s a -> s{_chsmrsStatus = a});
