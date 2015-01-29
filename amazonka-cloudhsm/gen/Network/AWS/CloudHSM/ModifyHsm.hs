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

-- Module      : Network.AWS.CloudHSM.ModifyHsm
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

-- | Modifies an HSM.
--
-- <http://docs.aws.amazon.com/cloudhsm/latest/dg/API_ModifyHsm.html>
module Network.AWS.CloudHSM.ModifyHsm
    (
    -- * Request
      ModifyHsm
    -- ** Request constructor
    , modifyHsm
    -- ** Request lenses
    , mhEniIp
    , mhExternalId
    , mhHsmArn
    , mhIamRoleArn
    , mhSubnetId
    , mhSyslogIp

    -- * Response
    , ModifyHsmResponse
    -- ** Response constructor
    , modifyHsmResponse
    -- ** Response lenses
    , mhrHsmArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CloudHSM.Types
import qualified GHC.Exts

data ModifyHsm = ModifyHsm
    { _mhEniIp      :: Maybe Text
    , _mhExternalId :: Maybe Text
    , _mhHsmArn     :: Text
    , _mhIamRoleArn :: Maybe Text
    , _mhSubnetId   :: Maybe Text
    , _mhSyslogIp   :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'ModifyHsm' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhEniIp' @::@ 'Maybe' 'Text'
--
-- * 'mhExternalId' @::@ 'Maybe' 'Text'
--
-- * 'mhHsmArn' @::@ 'Text'
--
-- * 'mhIamRoleArn' @::@ 'Maybe' 'Text'
--
-- * 'mhSubnetId' @::@ 'Maybe' 'Text'
--
-- * 'mhSyslogIp' @::@ 'Maybe' 'Text'
--
modifyHsm :: Text -- ^ 'mhHsmArn'
          -> ModifyHsm
modifyHsm p1 = ModifyHsm
    { _mhHsmArn     = p1
    , _mhSubnetId   = Nothing
    , _mhEniIp      = Nothing
    , _mhIamRoleArn = Nothing
    , _mhExternalId = Nothing
    , _mhSyslogIp   = Nothing
    }

-- | The new IP address for the elastic network interface attached to the HSM.
mhEniIp :: Lens' ModifyHsm (Maybe Text)
mhEniIp = lens _mhEniIp (\s a -> s { _mhEniIp = a })

-- | The new external ID.
mhExternalId :: Lens' ModifyHsm (Maybe Text)
mhExternalId = lens _mhExternalId (\s a -> s { _mhExternalId = a })

-- | The ARN of the HSM to modify.
mhHsmArn :: Lens' ModifyHsm Text
mhHsmArn = lens _mhHsmArn (\s a -> s { _mhHsmArn = a })

-- | The new IAM role ARN.
mhIamRoleArn :: Lens' ModifyHsm (Maybe Text)
mhIamRoleArn = lens _mhIamRoleArn (\s a -> s { _mhIamRoleArn = a })

-- | The new identifier of the subnet that the HSM is in.
mhSubnetId :: Lens' ModifyHsm (Maybe Text)
mhSubnetId = lens _mhSubnetId (\s a -> s { _mhSubnetId = a })

-- | The new IP address for the syslog monitoring server.
mhSyslogIp :: Lens' ModifyHsm (Maybe Text)
mhSyslogIp = lens _mhSyslogIp (\s a -> s { _mhSyslogIp = a })

newtype ModifyHsmResponse = ModifyHsmResponse
    { _mhrHsmArn :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'ModifyHsmResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mhrHsmArn' @::@ 'Maybe' 'Text'
--
modifyHsmResponse :: ModifyHsmResponse
modifyHsmResponse = ModifyHsmResponse
    { _mhrHsmArn = Nothing
    }

-- | The ARN of the HSM.
mhrHsmArn :: Lens' ModifyHsmResponse (Maybe Text)
mhrHsmArn = lens _mhrHsmArn (\s a -> s { _mhrHsmArn = a })

instance ToPath ModifyHsm where
    toPath = const "/"

instance ToQuery ModifyHsm where
    toQuery = const mempty

instance ToHeaders ModifyHsm

instance ToJSON ModifyHsm where
    toJSON ModifyHsm{..} = object
        [ "HsmArn"     .= _mhHsmArn
        , "SubnetId"   .= _mhSubnetId
        , "EniIp"      .= _mhEniIp
        , "IamRoleArn" .= _mhIamRoleArn
        , "ExternalId" .= _mhExternalId
        , "SyslogIp"   .= _mhSyslogIp
        ]

instance AWSRequest ModifyHsm where
    type Sv ModifyHsm = CloudHSM
    type Rs ModifyHsm = ModifyHsmResponse

    request  = post "ModifyHsm"
    response = jsonResponse

instance FromJSON ModifyHsmResponse where
    parseJSON = withObject "ModifyHsmResponse" $ \o -> ModifyHsmResponse
        <$> o .:? "HsmArn"
