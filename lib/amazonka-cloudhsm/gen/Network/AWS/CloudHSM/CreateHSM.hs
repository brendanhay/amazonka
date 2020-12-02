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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This is documentation for __AWS CloudHSM Classic__ . For more information, see <http://aws.amazon.com/cloudhsm/faqs-classic/ AWS CloudHSM Classic FAQs> , the <http://docs.aws.amazon.com/cloudhsm/classic/userguide/ AWS CloudHSM Classic User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/classic/APIReference/ AWS CloudHSM Classic API Reference> .
--
--
-- __For information about the current version of AWS CloudHSM__ , see <http://aws.amazon.com/cloudhsm/ AWS CloudHSM> , the <http://docs.aws.amazon.com/cloudhsm/latest/userguide/ AWS CloudHSM User Guide> , and the <http://docs.aws.amazon.com/cloudhsm/latest/APIReference/ AWS CloudHSM API Reference> .
--
-- Creates an uninitialized HSM instance.
--
-- There is an upfront fee charged for each HSM instance that you create with the @CreateHsm@ operation. If you accidentally provision an HSM and want to request a refund, delete the instance using the 'DeleteHsm' operation, go to the <https://console.aws.amazon.com/support/home AWS Support Center> , create a new case, and select __Account and Billing Support__ .
--
-- /Important:/ It can take up to 20 minutes to create and provision an HSM. You can monitor the status of the HSM with the 'DescribeHsm' operation. The HSM is ready to be initialized when the status changes to @RUNNING@ .
--
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
    , chrsHSMARN
    , chrsResponseStatus
    ) where

import Network.AWS.CloudHSM.Types
import Network.AWS.CloudHSM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the inputs for the @CreateHsm@ operation.
--
--
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHSM' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chClientToken' - A user-defined token to ensure idempotence. Subsequent calls to this operation with the same token will be ignored.
--
-- * 'chSyslogIP' - The IP address for the syslog monitoring server. The AWS CloudHSM service only supports one syslog monitoring server.
--
-- * 'chExternalId' - The external ID from @IamRoleArn@ , if present.
--
-- * 'chEniIP' - The IP address to assign to the HSM's ENI. If an IP address is not specified, an IP address will be randomly chosen from the CIDR range of the subnet.
--
-- * 'chSubnetId' - The identifier of the subnet in your VPC in which to place the HSM.
--
-- * 'chSSHKey' - The SSH public key to install on the HSM.
--
-- * 'chIAMRoleARN' - The ARN of an IAM role to enable the AWS CloudHSM service to allocate an ENI on your behalf.
--
-- * 'chSubscriptionType' - Undocumented member.
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


-- | A user-defined token to ensure idempotence. Subsequent calls to this operation with the same token will be ignored.
chClientToken :: Lens' CreateHSM (Maybe Text)
chClientToken = lens _chClientToken (\ s a -> s{_chClientToken = a})

-- | The IP address for the syslog monitoring server. The AWS CloudHSM service only supports one syslog monitoring server.
chSyslogIP :: Lens' CreateHSM (Maybe Text)
chSyslogIP = lens _chSyslogIP (\ s a -> s{_chSyslogIP = a})

-- | The external ID from @IamRoleArn@ , if present.
chExternalId :: Lens' CreateHSM (Maybe Text)
chExternalId = lens _chExternalId (\ s a -> s{_chExternalId = a})

-- | The IP address to assign to the HSM's ENI. If an IP address is not specified, an IP address will be randomly chosen from the CIDR range of the subnet.
chEniIP :: Lens' CreateHSM (Maybe Text)
chEniIP = lens _chEniIP (\ s a -> s{_chEniIP = a})

-- | The identifier of the subnet in your VPC in which to place the HSM.
chSubnetId :: Lens' CreateHSM Text
chSubnetId = lens _chSubnetId (\ s a -> s{_chSubnetId = a})

-- | The SSH public key to install on the HSM.
chSSHKey :: Lens' CreateHSM Text
chSSHKey = lens _chSSHKey (\ s a -> s{_chSSHKey = a})

-- | The ARN of an IAM role to enable the AWS CloudHSM service to allocate an ENI on your behalf.
chIAMRoleARN :: Lens' CreateHSM Text
chIAMRoleARN = lens _chIAMRoleARN (\ s a -> s{_chIAMRoleARN = a})

-- | Undocumented member.
chSubscriptionType :: Lens' CreateHSM SubscriptionType
chSubscriptionType = lens _chSubscriptionType (\ s a -> s{_chSubscriptionType = a})

instance AWSRequest CreateHSM where
        type Rs CreateHSM = CreateHSMResponse
        request = postJSON cloudHSM
        response
          = receiveJSON
              (\ s h x ->
                 CreateHSMResponse' <$>
                   (x .?> "HsmArn") <*> (pure (fromEnum s)))

instance Hashable CreateHSM where

instance NFData CreateHSM where

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

-- | Contains the output of the @CreateHsm@ operation.
--
--
--
-- /See:/ 'createHSMResponse' smart constructor.
data CreateHSMResponse = CreateHSMResponse'
  { _chrsHSMARN         :: !(Maybe Text)
  , _chrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateHSMResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'chrsHSMARN' - The ARN of the HSM.
--
-- * 'chrsResponseStatus' - -- | The response status code.
createHSMResponse
    :: Int -- ^ 'chrsResponseStatus'
    -> CreateHSMResponse
createHSMResponse pResponseStatus_ =
  CreateHSMResponse'
    {_chrsHSMARN = Nothing, _chrsResponseStatus = pResponseStatus_}


-- | The ARN of the HSM.
chrsHSMARN :: Lens' CreateHSMResponse (Maybe Text)
chrsHSMARN = lens _chrsHSMARN (\ s a -> s{_chrsHSMARN = a})

-- | -- | The response status code.
chrsResponseStatus :: Lens' CreateHSMResponse Int
chrsResponseStatus = lens _chrsResponseStatus (\ s a -> s{_chrsResponseStatus = a})

instance NFData CreateHSMResponse where
