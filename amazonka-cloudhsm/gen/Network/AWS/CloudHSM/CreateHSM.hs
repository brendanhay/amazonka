{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.CreateHSM
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an uninitialized HSM instance. Running this command provisions
-- an HSM appliance and will result in charges to your AWS account for the
-- HSM.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_CreateHSM.html>
module Network.AWS.CloudHSM.CreateHSM
    (
    -- * Request
      CreateHSM
    -- ** Request constructor
    , createHSM
    -- ** Request lenses
    , chrqClientToken
    , chrqSyslogIP
    , chrqExternalId
    , chrqEniIP
    , chrqSubnetId
    , chrqSSHKey
    , chrqIAMRoleARN
    , chrqSubscriptionType

    -- * Response
    , CreateHSMResponse
    -- ** Response constructor
    , createHSMResponse
    -- ** Response lenses
    , chsmrsHSMARN
    , chsmrsStatus
    ) where

import           Network.AWS.CloudHSM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the inputs for the CreateHsm action.
--
-- /See:/ 'createHSM' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chrqClientToken'
--
-- * 'chrqSyslogIP'
--
-- * 'chrqExternalId'
--
-- * 'chrqEniIP'
--
-- * 'chrqSubnetId'
--
-- * 'chrqSSHKey'
--
-- * 'chrqIAMRoleARN'
--
-- * 'chrqSubscriptionType'
data CreateHSM = CreateHSM'
    { _chrqClientToken      :: !(Maybe Text)
    , _chrqSyslogIP         :: !(Maybe Text)
    , _chrqExternalId       :: !(Maybe Text)
    , _chrqEniIP            :: !(Maybe Text)
    , _chrqSubnetId         :: !Text
    , _chrqSSHKey           :: !Text
    , _chrqIAMRoleARN       :: !Text
    , _chrqSubscriptionType :: !SubscriptionType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateHSM' smart constructor.
createHSM :: Text -> Text -> Text -> SubscriptionType -> CreateHSM
createHSM pSubnetId pSSHKey pIAMRoleARN pSubscriptionType =
    CreateHSM'
    { _chrqClientToken = Nothing
    , _chrqSyslogIP = Nothing
    , _chrqExternalId = Nothing
    , _chrqEniIP = Nothing
    , _chrqSubnetId = pSubnetId
    , _chrqSSHKey = pSSHKey
    , _chrqIAMRoleARN = pIAMRoleARN
    , _chrqSubscriptionType = pSubscriptionType
    }

-- | A user-defined token to ensure idempotence. Subsequent calls to this
-- action with the same token will be ignored.
chrqClientToken :: Lens' CreateHSM (Maybe Text)
chrqClientToken = lens _chrqClientToken (\ s a -> s{_chrqClientToken = a});

-- | The IP address for the syslog monitoring server.
chrqSyslogIP :: Lens' CreateHSM (Maybe Text)
chrqSyslogIP = lens _chrqSyslogIP (\ s a -> s{_chrqSyslogIP = a});

-- | The external ID from __IamRoleArn__, if present.
chrqExternalId :: Lens' CreateHSM (Maybe Text)
chrqExternalId = lens _chrqExternalId (\ s a -> s{_chrqExternalId = a});

-- | The IP address to assign to the HSM\'s ENI.
chrqEniIP :: Lens' CreateHSM (Maybe Text)
chrqEniIP = lens _chrqEniIP (\ s a -> s{_chrqEniIP = a});

-- | The identifier of the subnet in your VPC in which to place the HSM.
chrqSubnetId :: Lens' CreateHSM Text
chrqSubnetId = lens _chrqSubnetId (\ s a -> s{_chrqSubnetId = a});

-- | The SSH public key to install on the HSM.
chrqSSHKey :: Lens' CreateHSM Text
chrqSSHKey = lens _chrqSSHKey (\ s a -> s{_chrqSSHKey = a});

-- | The ARN of an IAM role to enable the AWS CloudHSM service to allocate an
-- ENI on your behalf.
chrqIAMRoleARN :: Lens' CreateHSM Text
chrqIAMRoleARN = lens _chrqIAMRoleARN (\ s a -> s{_chrqIAMRoleARN = a});

-- | The subscription type.
chrqSubscriptionType :: Lens' CreateHSM SubscriptionType
chrqSubscriptionType = lens _chrqSubscriptionType (\ s a -> s{_chrqSubscriptionType = a});

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
                    ("CloudHsmFrontendService.CreateHSM" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateHSM where
        toJSON CreateHSM'{..}
          = object
              ["ClientToken" .= _chrqClientToken,
               "SyslogIp" .= _chrqSyslogIP,
               "ExternalId" .= _chrqExternalId,
               "EniIp" .= _chrqEniIP, "SubnetId" .= _chrqSubnetId,
               "SshKey" .= _chrqSSHKey,
               "IamRoleArn" .= _chrqIAMRoleARN,
               "SubscriptionType" .= _chrqSubscriptionType]

instance ToPath CreateHSM where
        toPath = const "/"

instance ToQuery CreateHSM where
        toQuery = const mempty

-- | Contains the output of the CreateHsm action.
--
-- /See:/ 'createHSMResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chsmrsHSMARN'
--
-- * 'chsmrsStatus'
data CreateHSMResponse = CreateHSMResponse'
    { _chsmrsHSMARN :: !(Maybe Text)
    , _chsmrsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateHSMResponse' smart constructor.
createHSMResponse :: Int -> CreateHSMResponse
createHSMResponse pStatus =
    CreateHSMResponse'
    { _chsmrsHSMARN = Nothing
    , _chsmrsStatus = pStatus
    }

-- | The ARN of the HSM.
chsmrsHSMARN :: Lens' CreateHSMResponse (Maybe Text)
chsmrsHSMARN = lens _chsmrsHSMARN (\ s a -> s{_chsmrsHSMARN = a});

-- | FIXME: Undocumented member.
chsmrsStatus :: Lens' CreateHSMResponse Int
chsmrsStatus = lens _chsmrsStatus (\ s a -> s{_chsmrsStatus = a});
