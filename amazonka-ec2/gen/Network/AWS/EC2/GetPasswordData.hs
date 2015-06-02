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

-- Module      : Network.AWS.EC2.GetPasswordData
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

-- | Retrieves the encrypted administrator password for an instance running
-- Windows.
--
-- The Windows password is generated at boot if the 'EC2Config' service plugin, 'Ec2SetPassword', is enabled. This usually only happens the first time an AMI is launched,
-- and then 'Ec2SetPassword' is automatically disabled. The password is not
-- generated for rebundled AMIs unless 'Ec2SetPassword' is enabled before bundling.
--
-- The password is encrypted using the key pair that you specified when you
-- launched the instance. You must provide the corresponding key pair file.
--
-- Password generation and encryption takes a few moments. We recommend that
-- you wait up to 15 minutes after launching an instance before trying to
-- retrieve the generated password.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-GetPasswordData.html>
module Network.AWS.EC2.GetPasswordData
    (
    -- * Request
      GetPasswordData
    -- ** Request constructor
    , getPasswordData
    -- ** Request lenses
    , gpdDryRun
    , gpdInstanceId

    -- * Response
    , GetPasswordDataResponse
    -- ** Response constructor
    , getPasswordDataResponse
    -- ** Response lenses
    , gpdrInstanceId
    , gpdrPasswordData
    , gpdrTimestamp
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data GetPasswordData = GetPasswordData
    { _gpdDryRun     :: Maybe Bool
    , _gpdInstanceId :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'GetPasswordData' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpdDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'gpdInstanceId' @::@ 'Text'
--
getPasswordData :: Text -- ^ 'gpdInstanceId'
                -> GetPasswordData
getPasswordData p1 = GetPasswordData
    { _gpdInstanceId = p1
    , _gpdDryRun     = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
gpdDryRun :: Lens' GetPasswordData (Maybe Bool)
gpdDryRun = lens _gpdDryRun (\s a -> s { _gpdDryRun = a })

-- | The ID of the Windows instance.
gpdInstanceId :: Lens' GetPasswordData Text
gpdInstanceId = lens _gpdInstanceId (\s a -> s { _gpdInstanceId = a })

data GetPasswordDataResponse = GetPasswordDataResponse
    { _gpdrInstanceId   :: Text
    , _gpdrPasswordData :: Text
    , _gpdrTimestamp    :: ISO8601
    } deriving (Eq, Ord, Read, Show)

-- | 'GetPasswordDataResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpdrInstanceId' @::@ 'Text'
--
-- * 'gpdrPasswordData' @::@ 'Text'
--
-- * 'gpdrTimestamp' @::@ 'UTCTime'
--
getPasswordDataResponse :: Text -- ^ 'gpdrInstanceId'
                        -> UTCTime -- ^ 'gpdrTimestamp'
                        -> Text -- ^ 'gpdrPasswordData'
                        -> GetPasswordDataResponse
getPasswordDataResponse p1 p2 p3 = GetPasswordDataResponse
    { _gpdrInstanceId   = p1
    , _gpdrTimestamp    = withIso _Time (const id) p2
    , _gpdrPasswordData = p3
    }

-- | The ID of the Windows instance.
gpdrInstanceId :: Lens' GetPasswordDataResponse Text
gpdrInstanceId = lens _gpdrInstanceId (\s a -> s { _gpdrInstanceId = a })

-- | The password of the instance.
gpdrPasswordData :: Lens' GetPasswordDataResponse Text
gpdrPasswordData = lens _gpdrPasswordData (\s a -> s { _gpdrPasswordData = a })

-- | The time the data was last updated.
gpdrTimestamp :: Lens' GetPasswordDataResponse UTCTime
gpdrTimestamp = lens _gpdrTimestamp (\s a -> s { _gpdrTimestamp = a }) . _Time

instance ToPath GetPasswordData where
    toPath = const "/"

instance ToQuery GetPasswordData where
    toQuery GetPasswordData{..} = mconcat
        [ "DryRun"     =? _gpdDryRun
        , "InstanceId" =? _gpdInstanceId
        ]

instance ToHeaders GetPasswordData

instance AWSRequest GetPasswordData where
    type Sv GetPasswordData = EC2
    type Rs GetPasswordData = GetPasswordDataResponse

    request  = post "GetPasswordData"
    response = xmlResponse

instance FromXML GetPasswordDataResponse where
    parseXML x = GetPasswordDataResponse
        <$> x .@  "instanceId"
        <*> x .@  "passwordData"
        <*> x .@  "timestamp"
