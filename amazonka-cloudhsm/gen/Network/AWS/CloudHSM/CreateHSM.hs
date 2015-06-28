{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.CloudHSM.CreateHSM
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates an uninitialized HSM instance. Running this command provisions
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
    , chClientToken
    , chSyslogIP
    , chExternalId
    , chEniIP
    , chSubnetId
    , chSSHKey
    , chIAMRoleARN
    , chSubscriptionType

    -- * Response
    , CreateHSMResponse
    -- ** Response constructor
    , createHSMResponse
    -- ** Response lenses
    , creHSMARN
    , creStatus
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
data CreateHSM = CreateHSM'
    { _chClientToken      :: !(Maybe Text)
    , _chSyslogIP         :: !(Maybe Text)
    , _chExternalId       :: !(Maybe Text)
    , _chEniIP            :: !(Maybe Text)
    , _chSubnetId         :: !Text
    , _chSSHKey           :: !Text
    , _chIAMRoleARN       :: !Text
    , _chSubscriptionType :: !SubscriptionType
    } deriving (Eq,Read,Show)

-- | 'CreateHSM' smart constructor.
createHSM :: Text -> Text -> Text -> SubscriptionType -> CreateHSM
createHSM pSubnetId pSSHKey pIAMRoleARN pSubscriptionType =
    CreateHSM'
    { _chClientToken = Nothing
    , _chSyslogIP = Nothing
    , _chExternalId = Nothing
    , _chEniIP = Nothing
    , _chSubnetId = pSubnetId
    , _chSSHKey = pSSHKey
    , _chIAMRoleARN = pIAMRoleARN
    , _chSubscriptionType = pSubscriptionType
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
                 CreateHSMResponse' <$> (x .?> "HsmArn") <*> (pure s))

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
              ["ClientToken" .= _chClientToken,
               "SyslogIp" .= _chSyslogIP,
               "ExternalId" .= _chExternalId, "EniIp" .= _chEniIP,
               "SubnetId" .= _chSubnetId, "SshKey" .= _chSSHKey,
               "IamRoleArn" .= _chIAMRoleARN,
               "SubscriptionType" .= _chSubscriptionType]

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
-- * 'creHSMARN'
--
-- * 'creStatus'
data CreateHSMResponse = CreateHSMResponse'
    { _creHSMARN :: !(Maybe Text)
    , _creStatus :: !Status
    } deriving (Eq,Read,Show)

-- | 'CreateHSMResponse' smart constructor.
createHSMResponse :: Status -> CreateHSMResponse
createHSMResponse pStatus =
    CreateHSMResponse'
    { _creHSMARN = Nothing
    , _creStatus = pStatus
    }

-- | The ARN of the HSM.
creHSMARN :: Lens' CreateHSMResponse (Maybe Text)
creHSMARN = lens _creHSMARN (\ s a -> s{_creHSMARN = a});

-- | FIXME: Undocumented member.
creStatus :: Lens' CreateHSMResponse Status
creStatus = lens _creStatus (\ s a -> s{_creStatus = a});
