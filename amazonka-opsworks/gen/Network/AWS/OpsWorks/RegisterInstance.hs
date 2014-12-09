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

-- Module      : Network.AWS.OpsWorks.RegisterInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Registers instances with a specified stack that were created outside of AWS
-- OpsWorks.
--
-- We do not recommend using this action to register instances. The complete
-- registration operation has two primary steps, installing the AWS OpsWorks
-- agent on the instance and registering the instance with the stack. 'RegisterInstance' handles only the second step. You should instead use the AWS CLI 'register'
-- command, which performs the entire registration operation. Required
-- Permissions: To use this action, an IAM user must have a Manage permissions
-- level for the stack or an attached policy that explicitly grants permissions.
-- For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_RegisterInstance.html>
module Network.AWS.OpsWorks.RegisterInstance
    (
    -- * Request
      RegisterInstance
    -- ** Request constructor
    , registerInstance
    -- ** Request lenses
    , riHostname
    , riInstanceIdentity
    , riPrivateIp
    , riPublicIp
    , riRsaPublicKey
    , riRsaPublicKeyFingerprint
    , riStackId

    -- * Response
    , RegisterInstanceResponse
    -- ** Response constructor
    , registerInstanceResponse
    -- ** Response lenses
    , rirInstanceId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.OpsWorks.Types
import qualified GHC.Exts

data RegisterInstance = RegisterInstance
    { _riHostname                :: Maybe Text
    , _riInstanceIdentity        :: Maybe InstanceIdentity
    , _riPrivateIp               :: Maybe Text
    , _riPublicIp                :: Maybe Text
    , _riRsaPublicKey            :: Maybe Text
    , _riRsaPublicKeyFingerprint :: Maybe Text
    , _riStackId                 :: Text
    } deriving (Eq, Show)

-- | 'RegisterInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riHostname' @::@ 'Maybe' 'Text'
--
-- * 'riInstanceIdentity' @::@ 'Maybe' 'InstanceIdentity'
--
-- * 'riPrivateIp' @::@ 'Maybe' 'Text'
--
-- * 'riPublicIp' @::@ 'Maybe' 'Text'
--
-- * 'riRsaPublicKey' @::@ 'Maybe' 'Text'
--
-- * 'riRsaPublicKeyFingerprint' @::@ 'Maybe' 'Text'
--
-- * 'riStackId' @::@ 'Text'
--
registerInstance :: Text -- ^ 'riStackId'
                 -> RegisterInstance
registerInstance p1 = RegisterInstance
    { _riStackId                 = p1
    , _riHostname                = Nothing
    , _riPublicIp                = Nothing
    , _riPrivateIp               = Nothing
    , _riRsaPublicKey            = Nothing
    , _riRsaPublicKeyFingerprint = Nothing
    , _riInstanceIdentity        = Nothing
    }

-- | The instance's hostname.
riHostname :: Lens' RegisterInstance (Maybe Text)
riHostname = lens _riHostname (\s a -> s { _riHostname = a })

-- | An InstanceIdentity object that contains the instance's identity.
riInstanceIdentity :: Lens' RegisterInstance (Maybe InstanceIdentity)
riInstanceIdentity =
    lens _riInstanceIdentity (\s a -> s { _riInstanceIdentity = a })

-- | The instance's private IP address.
riPrivateIp :: Lens' RegisterInstance (Maybe Text)
riPrivateIp = lens _riPrivateIp (\s a -> s { _riPrivateIp = a })

-- | The instance's public IP address.
riPublicIp :: Lens' RegisterInstance (Maybe Text)
riPublicIp = lens _riPublicIp (\s a -> s { _riPublicIp = a })

-- | The instances public RSA key. This key is used to encrypt communication
-- between the instance and the service.
riRsaPublicKey :: Lens' RegisterInstance (Maybe Text)
riRsaPublicKey = lens _riRsaPublicKey (\s a -> s { _riRsaPublicKey = a })

-- | The instances public RSA key fingerprint.
riRsaPublicKeyFingerprint :: Lens' RegisterInstance (Maybe Text)
riRsaPublicKeyFingerprint =
    lens _riRsaPublicKeyFingerprint
        (\s a -> s { _riRsaPublicKeyFingerprint = a })

-- | The ID of the stack that the instance is to be registered with.
riStackId :: Lens' RegisterInstance Text
riStackId = lens _riStackId (\s a -> s { _riStackId = a })

newtype RegisterInstanceResponse = RegisterInstanceResponse
    { _rirInstanceId :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'RegisterInstanceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rirInstanceId' @::@ 'Maybe' 'Text'
--
registerInstanceResponse :: RegisterInstanceResponse
registerInstanceResponse = RegisterInstanceResponse
    { _rirInstanceId = Nothing
    }

-- | The registered instance's AWS OpsWorks ID.
rirInstanceId :: Lens' RegisterInstanceResponse (Maybe Text)
rirInstanceId = lens _rirInstanceId (\s a -> s { _rirInstanceId = a })

instance ToPath RegisterInstance where
    toPath = const "/"

instance ToQuery RegisterInstance where
    toQuery = const mempty

instance ToHeaders RegisterInstance

instance ToJSON RegisterInstance where
    toJSON RegisterInstance{..} = object
        [ "StackId"                 .= _riStackId
        , "Hostname"                .= _riHostname
        , "PublicIp"                .= _riPublicIp
        , "PrivateIp"               .= _riPrivateIp
        , "RsaPublicKey"            .= _riRsaPublicKey
        , "RsaPublicKeyFingerprint" .= _riRsaPublicKeyFingerprint
        , "InstanceIdentity"        .= _riInstanceIdentity
        ]

instance AWSRequest RegisterInstance where
    type Sv RegisterInstance = OpsWorks
    type Rs RegisterInstance = RegisterInstanceResponse

    request  = post "RegisterInstance"
    response = jsonResponse

instance FromJSON RegisterInstanceResponse where
    parseJSON = withObject "RegisterInstanceResponse" $ \o -> RegisterInstanceResponse
        <$> o .:? "InstanceId"
