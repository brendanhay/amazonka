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
-- Module      : Network.AWS.OpsWorks.RegisterInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers instances with a specified stack that were created outside of
-- AWS OpsWorks.
--
-- We do not recommend using this action to register instances. The
-- complete registration operation has two primary steps, installing the
-- AWS OpsWorks agent on the instance and registering the instance with the
-- stack. 'RegisterInstance' handles only the second step. You should
-- instead use the AWS CLI 'register' command, which performs the entire
-- registration operation. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/registered-instances-register.html Registering an Instance with an AWS OpsWorks Stack>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- /See:/ <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterInstance.html AWS API Reference> for RegisterInstance.
module Network.AWS.OpsWorks.RegisterInstance
    (
    -- * Creating a Request
      registerInstance
    , RegisterInstance
    -- * Request Lenses
    , riPrivateIP
    , riHostname
    , riInstanceIdentity
    , riPublicIP
    , riRsaPublicKeyFingerprint
    , riRsaPublicKey
    , riStackId

    -- * Destructuring the Response
    , registerInstanceResponse
    , RegisterInstanceResponse
    -- * Response Lenses
    , rirsInstanceId
    , rirsStatus
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.OpsWorks.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'registerInstance' smart constructor.
data RegisterInstance = RegisterInstance'
    { _riPrivateIP               :: !(Maybe Text)
    , _riHostname                :: !(Maybe Text)
    , _riInstanceIdentity        :: !(Maybe InstanceIdentity)
    , _riPublicIP                :: !(Maybe Text)
    , _riRsaPublicKeyFingerprint :: !(Maybe Text)
    , _riRsaPublicKey            :: !(Maybe Text)
    , _riStackId                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
registerInstance
    :: Text -- ^ 'riStackId'
    -> RegisterInstance
registerInstance pStackId_ =
    RegisterInstance'
    { _riPrivateIP = Nothing
    , _riHostname = Nothing
    , _riInstanceIdentity = Nothing
    , _riPublicIP = Nothing
    , _riRsaPublicKeyFingerprint = Nothing
    , _riRsaPublicKey = Nothing
    , _riStackId = pStackId_
    }

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

-- | Contains the response to a 'RegisterInstanceResult' request.
--
-- /See:/ 'registerInstanceResponse' smart constructor.
data RegisterInstanceResponse = RegisterInstanceResponse'
    { _rirsInstanceId :: !(Maybe Text)
    , _rirsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'RegisterInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rirsInstanceId'
--
-- * 'rirsStatus'
registerInstanceResponse
    :: Int -- ^ 'rirsStatus'
    -> RegisterInstanceResponse
registerInstanceResponse pStatus_ =
    RegisterInstanceResponse'
    { _rirsInstanceId = Nothing
    , _rirsStatus = pStatus_
    }

-- | The registered instance\'s AWS OpsWorks ID.
rirsInstanceId :: Lens' RegisterInstanceResponse (Maybe Text)
rirsInstanceId = lens _rirsInstanceId (\ s a -> s{_rirsInstanceId = a});

-- | The response status code.
rirsStatus :: Lens' RegisterInstanceResponse Int
rirsStatus = lens _rirsStatus (\ s a -> s{_rirsStatus = a});
