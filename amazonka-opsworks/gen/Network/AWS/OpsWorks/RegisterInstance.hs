{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.RegisterInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Registers instances with a specified stack that were created outside of
-- AWS OpsWorks.
--
-- We do not recommend using this action to register instances. The
-- complete registration operation has two primary steps, installing the
-- AWS OpsWorks agent on the instance and registering the instance with the
-- stack. @RegisterInstance@ handles only the second step. You should
-- instead use the AWS CLI @register@ command, which performs the entire
-- registration operation. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/registered-instances-register.html Registering an Instance with an AWS OpsWorks Stack>.
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
    , rirqPrivateIP
    , rirqHostname
    , rirqInstanceIdentity
    , rirqPublicIP
    , rirqRsaPublicKeyFingerprint
    , rirqRsaPublicKey
    , rirqStackId

    -- * Response
    , RegisterInstanceResponse
    -- ** Response constructor
    , registerInstanceResponse
    -- ** Response lenses
    , rirsInstanceId
    , rirsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerInstance' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rirqPrivateIP'
--
-- * 'rirqHostname'
--
-- * 'rirqInstanceIdentity'
--
-- * 'rirqPublicIP'
--
-- * 'rirqRsaPublicKeyFingerprint'
--
-- * 'rirqRsaPublicKey'
--
-- * 'rirqStackId'
data RegisterInstance = RegisterInstance'
    { _rirqPrivateIP               :: !(Maybe Text)
    , _rirqHostname                :: !(Maybe Text)
    , _rirqInstanceIdentity        :: !(Maybe InstanceIdentity)
    , _rirqPublicIP                :: !(Maybe Text)
    , _rirqRsaPublicKeyFingerprint :: !(Maybe Text)
    , _rirqRsaPublicKey            :: !(Maybe Text)
    , _rirqStackId                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterInstance' smart constructor.
registerInstance :: Text -> RegisterInstance
registerInstance pStackId =
    RegisterInstance'
    { _rirqPrivateIP = Nothing
    , _rirqHostname = Nothing
    , _rirqInstanceIdentity = Nothing
    , _rirqPublicIP = Nothing
    , _rirqRsaPublicKeyFingerprint = Nothing
    , _rirqRsaPublicKey = Nothing
    , _rirqStackId = pStackId
    }

-- | The instance\'s private IP address.
rirqPrivateIP :: Lens' RegisterInstance (Maybe Text)
rirqPrivateIP = lens _rirqPrivateIP (\ s a -> s{_rirqPrivateIP = a});

-- | The instance\'s hostname.
rirqHostname :: Lens' RegisterInstance (Maybe Text)
rirqHostname = lens _rirqHostname (\ s a -> s{_rirqHostname = a});

-- | An InstanceIdentity object that contains the instance\'s identity.
rirqInstanceIdentity :: Lens' RegisterInstance (Maybe InstanceIdentity)
rirqInstanceIdentity = lens _rirqInstanceIdentity (\ s a -> s{_rirqInstanceIdentity = a});

-- | The instance\'s public IP address.
rirqPublicIP :: Lens' RegisterInstance (Maybe Text)
rirqPublicIP = lens _rirqPublicIP (\ s a -> s{_rirqPublicIP = a});

-- | The instances public RSA key fingerprint.
rirqRsaPublicKeyFingerprint :: Lens' RegisterInstance (Maybe Text)
rirqRsaPublicKeyFingerprint = lens _rirqRsaPublicKeyFingerprint (\ s a -> s{_rirqRsaPublicKeyFingerprint = a});

-- | The instances public RSA key. This key is used to encrypt communication
-- between the instance and the service.
rirqRsaPublicKey :: Lens' RegisterInstance (Maybe Text)
rirqRsaPublicKey = lens _rirqRsaPublicKey (\ s a -> s{_rirqRsaPublicKey = a});

-- | The ID of the stack that the instance is to be registered with.
rirqStackId :: Lens' RegisterInstance Text
rirqStackId = lens _rirqStackId (\ s a -> s{_rirqStackId = a});

instance AWSRequest RegisterInstance where
        type Sv RegisterInstance = OpsWorks
        type Rs RegisterInstance = RegisterInstanceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 RegisterInstanceResponse' <$>
                   (x .?> "InstanceId") <*> (pure (fromEnum s)))

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
              ["PrivateIp" .= _rirqPrivateIP,
               "Hostname" .= _rirqHostname,
               "InstanceIdentity" .= _rirqInstanceIdentity,
               "PublicIp" .= _rirqPublicIP,
               "RsaPublicKeyFingerprint" .=
                 _rirqRsaPublicKeyFingerprint,
               "RsaPublicKey" .= _rirqRsaPublicKey,
               "StackId" .= _rirqStackId]

instance ToPath RegisterInstance where
        toPath = const "/"

instance ToQuery RegisterInstance where
        toQuery = const mempty

-- | Contains the response to a @RegisterInstanceResult@ request.
--
-- /See:/ 'registerInstanceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rirsInstanceId'
--
-- * 'rirsStatus'
data RegisterInstanceResponse = RegisterInstanceResponse'
    { _rirsInstanceId :: !(Maybe Text)
    , _rirsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RegisterInstanceResponse' smart constructor.
registerInstanceResponse :: Int -> RegisterInstanceResponse
registerInstanceResponse pStatus =
    RegisterInstanceResponse'
    { _rirsInstanceId = Nothing
    , _rirsStatus = pStatus
    }

-- | The registered instance\'s AWS OpsWorks ID.
rirsInstanceId :: Lens' RegisterInstanceResponse (Maybe Text)
rirsInstanceId = lens _rirsInstanceId (\ s a -> s{_rirsInstanceId = a});

-- | FIXME: Undocumented member.
rirsStatus :: Lens' RegisterInstanceResponse Int
rirsStatus = lens _rirsStatus (\ s a -> s{_rirsStatus = a});
