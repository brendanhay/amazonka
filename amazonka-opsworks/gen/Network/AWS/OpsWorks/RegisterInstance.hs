{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.OpsWorks.RegisterInstance
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

-- | Registers instances with a specified stack that were created outside of
-- AWS OpsWorks.
--
-- We do not recommend using this action to register instances. The
-- complete registration operation has two primary steps, installing the
-- AWS OpsWorks agent on the instance and registering the instance with the
-- stack. @RegisterInstance@ handles only the second step. You should
-- instead use the AWS CLI @register@ command, which performs the entire
-- registration operation.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterInstance.html>
module Network.AWS.OpsWorks.RegisterInstance
    (
    -- * Request
      RegisterInstance
    -- ** Request constructor
    , registerInstance
    -- ** Request lenses
    , riPrivateIP
    , riHostname
    , riInstanceIdentity
    , riPublicIP
    , riRsaPublicKeyFingerprint
    , riRsaPublicKey
    , riStackId

    -- * Response
    , RegisterInstanceResponse
    -- ** Response constructor
    , registerInstanceResponse
    -- ** Response lenses
    , rirInstanceId
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.OpsWorks.Types

-- | /See:/ 'registerInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riPrivateIP'
--
-- * 'riHostname'
--
-- * 'riInstanceIdentity'
--
-- * 'riPublicIP'
--
-- * 'riRsaPublicKeyFingerprint'
--
-- * 'riRsaPublicKey'
--
-- * 'riStackId'
data RegisterInstance = RegisterInstance'{_riPrivateIP :: Maybe Text, _riHostname :: Maybe Text, _riInstanceIdentity :: Maybe InstanceIdentity, _riPublicIP :: Maybe Text, _riRsaPublicKeyFingerprint :: Maybe Text, _riRsaPublicKey :: Maybe Text, _riStackId :: Text} deriving (Eq, Read, Show)

-- | 'RegisterInstance' smart constructor.
registerInstance :: Text -> RegisterInstance
registerInstance pStackId = RegisterInstance'{_riPrivateIP = Nothing, _riHostname = Nothing, _riInstanceIdentity = Nothing, _riPublicIP = Nothing, _riRsaPublicKeyFingerprint = Nothing, _riRsaPublicKey = Nothing, _riStackId = pStackId};

-- | The instance\'s private IP address.
riPrivateIP :: Lens' RegisterInstance (Maybe Text)
riPrivateIP = lens _riPrivateIP (\ s a -> s{_riPrivateIP = a});

-- | The instance\'s hostname.
riHostname :: Lens' RegisterInstance (Maybe Text)
riHostname = lens _riHostname (\ s a -> s{_riHostname = a});

-- | An InstanceIdentity object that contains the instance\'s identity.
riInstanceIdentity :: Lens' RegisterInstance (Maybe InstanceIdentity)
riInstanceIdentity = lens _riInstanceIdentity (\ s a -> s{_riInstanceIdentity = a});

-- | The instance\'s public IP address.
riPublicIP :: Lens' RegisterInstance (Maybe Text)
riPublicIP = lens _riPublicIP (\ s a -> s{_riPublicIP = a});

-- | The instances public RSA key fingerprint.
riRsaPublicKeyFingerprint :: Lens' RegisterInstance (Maybe Text)
riRsaPublicKeyFingerprint = lens _riRsaPublicKeyFingerprint (\ s a -> s{_riRsaPublicKeyFingerprint = a});

-- | The instances public RSA key. This key is used to encrypt communication
-- between the instance and the service.
riRsaPublicKey :: Lens' RegisterInstance (Maybe Text)
riRsaPublicKey = lens _riRsaPublicKey (\ s a -> s{_riRsaPublicKey = a});

-- | The ID of the stack that the instance is to be registered with.
riStackId :: Lens' RegisterInstance Text
riStackId = lens _riStackId (\ s a -> s{_riStackId = a});

instance AWSRequest RegisterInstance where
        type Sv RegisterInstance = OpsWorks
        type Rs RegisterInstance = RegisterInstanceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterInstanceResponse' <$> x .?> "InstanceId")

instance ToHeaders RegisterInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.RegisterInstance" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterInstance where
        toJSON RegisterInstance'{..}
          = object
              ["PrivateIp" .= _riPrivateIP,
               "Hostname" .= _riHostname,
               "InstanceIdentity" .= _riInstanceIdentity,
               "PublicIp" .= _riPublicIP,
               "RsaPublicKeyFingerprint" .=
                 _riRsaPublicKeyFingerprint,
               "RsaPublicKey" .= _riRsaPublicKey,
               "StackId" .= _riStackId]

instance ToPath RegisterInstance where
        toPath = const "/"

instance ToQuery RegisterInstance where
        toQuery = const mempty

-- | /See:/ 'registerInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rirInstanceId'
newtype RegisterInstanceResponse = RegisterInstanceResponse'{_rirInstanceId :: Maybe Text} deriving (Eq, Read, Show)

-- | 'RegisterInstanceResponse' smart constructor.
registerInstanceResponse :: RegisterInstanceResponse
registerInstanceResponse = RegisterInstanceResponse'{_rirInstanceId = Nothing};

-- | The registered instance\'s AWS OpsWorks ID.
rirInstanceId :: Lens' RegisterInstanceResponse (Maybe Text)
rirInstanceId = lens _rirInstanceId (\ s a -> s{_rirInstanceId = a});
