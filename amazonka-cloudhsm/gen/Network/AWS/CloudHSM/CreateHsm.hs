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

-- Module      : Network.AWS.CloudHSM.CreateHsm
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

-- | Creates an uninitialized HSM instance. Running this command provisions an HSM
-- appliance and will result in charges to your AWS account for the HSM.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_CreateHsm.html>
module Network.AWS.CloudHSM.CreateHsm
    (
    -- * Request
      CreateHsm
    -- ** Request constructor
    , createHsm
    -- ** Request lenses
    , chClientToken
    , chEniIp
    , chExternalId
    , chIamRoleArn
    , chSshKey
    , chSubnetId
    , chSubscriptionType
    , chSyslogIp

    -- * Response
    , CreateHsmResponse
    -- ** Response constructor
    , createHsmResponse
    -- ** Response lenses
    , chrHsmArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

data CreateHsm = CreateHsm
    { _chClientToken      :: Maybe Text
    , _chEniIp            :: Maybe Text
    , _chExternalId       :: Maybe Text
    , _chIamRoleArn       :: Text
    , _chSshKey           :: Text
    , _chSubnetId         :: Text
    , _chSubscriptionType :: SubscriptionType
    , _chSyslogIp         :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'CreateHsm' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chClientToken' @::@ 'Maybe' 'Text'
--
-- * 'chEniIp' @::@ 'Maybe' 'Text'
--
-- * 'chExternalId' @::@ 'Maybe' 'Text'
--
-- * 'chIamRoleArn' @::@ 'Text'
--
-- * 'chSshKey' @::@ 'Text'
--
-- * 'chSubnetId' @::@ 'Text'
--
-- * 'chSubscriptionType' @::@ 'SubscriptionType'
--
-- * 'chSyslogIp' @::@ 'Maybe' 'Text'
--
createHsm :: Text -- ^ 'chSubnetId'
          -> Text -- ^ 'chSshKey'
          -> Text -- ^ 'chIamRoleArn'
          -> SubscriptionType -- ^ 'chSubscriptionType'
          -> CreateHsm
createHsm p1 p2 p3 p4 = CreateHsm
    { _chSubnetId         = p1
    , _chSshKey           = p2
    , _chIamRoleArn       = p3
    , _chSubscriptionType = p4
    , _chEniIp            = Nothing
    , _chExternalId       = Nothing
    , _chClientToken      = Nothing
    , _chSyslogIp         = Nothing
    }

-- | A user-defined token to ensure idempotence. Subsequent calls to this action
-- with the same token will be ignored.
chClientToken :: Lens' CreateHsm (Maybe Text)
chClientToken = lens _chClientToken (\s a -> s { _chClientToken = a })

-- | The IP address to assign to the HSM's ENI.
chEniIp :: Lens' CreateHsm (Maybe Text)
chEniIp = lens _chEniIp (\s a -> s { _chEniIp = a })

-- | The external ID from IamRoleArn, if present.
chExternalId :: Lens' CreateHsm (Maybe Text)
chExternalId = lens _chExternalId (\s a -> s { _chExternalId = a })

-- | The ARN of an IAM role to enable the AWS CloudHSM service to allocate an ENI
-- on your behalf.
chIamRoleArn :: Lens' CreateHsm Text
chIamRoleArn = lens _chIamRoleArn (\s a -> s { _chIamRoleArn = a })

-- | The SSH public key to install on the HSM.
chSshKey :: Lens' CreateHsm Text
chSshKey = lens _chSshKey (\s a -> s { _chSshKey = a })

-- | The identifier of the subnet in your VPC in which to place the HSM.
chSubnetId :: Lens' CreateHsm Text
chSubnetId = lens _chSubnetId (\s a -> s { _chSubnetId = a })

-- | The subscription type.
chSubscriptionType :: Lens' CreateHsm SubscriptionType
chSubscriptionType =
    lens _chSubscriptionType (\s a -> s { _chSubscriptionType = a })

-- | The IP address for the syslog monitoring server.
chSyslogIp :: Lens' CreateHsm (Maybe Text)
chSyslogIp = lens _chSyslogIp (\s a -> s { _chSyslogIp = a })

newtype CreateHsmResponse = CreateHsmResponse
    { _chrHsmArn :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'CreateHsmResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'chrHsmArn' @::@ 'Maybe' 'Text'
--
createHsmResponse :: CreateHsmResponse
createHsmResponse = CreateHsmResponse
    { _chrHsmArn = Nothing
    }

-- | The ARN of the HSM.
chrHsmArn :: Lens' CreateHsmResponse (Maybe Text)
chrHsmArn = lens _chrHsmArn (\s a -> s { _chrHsmArn = a })

instance ToPath CreateHsm where
    toPath = const "/"

instance ToQuery CreateHsm where
    toQuery = const mempty

instance ToHeaders CreateHsm

instance ToJSON CreateHsm where
    toJSON CreateHsm{..} = object
        [ "SubnetId"         .= _chSubnetId
        , "SshKey"           .= _chSshKey
        , "EniIp"            .= _chEniIp
        , "IamRoleArn"       .= _chIamRoleArn
        , "ExternalId"       .= _chExternalId
        , "SubscriptionType" .= _chSubscriptionType
        , "ClientToken"      .= _chClientToken
        , "SyslogIp"         .= _chSyslogIp
        ]

instance AWSRequest CreateHsm where
    type Sv CreateHsm = CloudHSM
    type Rs CreateHsm = CreateHsmResponse

    request  = post "CreateHsm"
    response = jsonResponse

instance FromJSON CreateHsmResponse where
    parseJSON = withObject "CreateHsmResponse" $ \o -> CreateHsmResponse
        <$> o .:? "HsmArn"
