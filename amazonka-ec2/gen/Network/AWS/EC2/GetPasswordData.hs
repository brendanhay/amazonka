{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.GetPasswordData
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the encrypted administrator password for an instance running
-- Windows. The Windows password is generated at boot if the EC2Config service
-- plugin, Ec2SetPassword, is enabled. This usually only happens the first
-- time an AMI is launched, and then Ec2SetPassword is automatically disabled.
-- The password is not generated for rebundled AMIs unless Ec2SetPassword is
-- enabled before bundling. The password is encrypted using the key pair that
-- you specified when you launched the instance. You must provide the
-- corresponding key pair file. Password generation and encryption takes a few
-- moments. We recommend that you wait up to 15 minutes after launching an
-- instance before trying to retrieve the generated password.
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
    , GetPasswordDataResult
    -- ** Response constructor
    , getPasswordDataResult
    -- ** Response lenses
    , gpdrInstanceId
    , gpdrPasswordData
    , gpdrTimestamp
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data GetPasswordData = GetPasswordData
    { _gpdDryRun     :: Maybe Bool
    , _gpdInstanceId :: Text
    } deriving (Eq, Ord, Show, Generic)

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

gpdDryRun :: Lens' GetPasswordData (Maybe Bool)
gpdDryRun = lens _gpdDryRun (\s a -> s { _gpdDryRun = a })

-- | The ID of the Windows instance.
gpdInstanceId :: Lens' GetPasswordData Text
gpdInstanceId = lens _gpdInstanceId (\s a -> s { _gpdInstanceId = a })

instance ToPath GetPasswordData where
    toPath = const "/"

instance ToQuery GetPasswordData

data GetPasswordDataResult = GetPasswordDataResult
    { _gpdrInstanceId   :: Maybe Text
    , _gpdrPasswordData :: Maybe Text
    , _gpdrTimestamp    :: Maybe RFC822
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetPasswordDataResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpdrInstanceId' @::@ 'Maybe' 'Text'
--
-- * 'gpdrPasswordData' @::@ 'Maybe' 'Text'
--
-- * 'gpdrTimestamp' @::@ 'Maybe' 'UTCTime'
--
getPasswordDataResult :: GetPasswordDataResult
getPasswordDataResult = GetPasswordDataResult
    { _gpdrInstanceId   = Nothing
    , _gpdrTimestamp    = Nothing
    , _gpdrPasswordData = Nothing
    }

-- | The ID of the Windows instance.
gpdrInstanceId :: Lens' GetPasswordDataResult (Maybe Text)
gpdrInstanceId = lens _gpdrInstanceId (\s a -> s { _gpdrInstanceId = a })

-- | The password of the instance.
gpdrPasswordData :: Lens' GetPasswordDataResult (Maybe Text)
gpdrPasswordData = lens _gpdrPasswordData (\s a -> s { _gpdrPasswordData = a })

-- | The time the data was last updated.
gpdrTimestamp :: Lens' GetPasswordDataResult (Maybe UTCTime)
gpdrTimestamp = lens _gpdrTimestamp (\s a -> s { _gpdrTimestamp = a })
    . mapping _Time

instance AWSRequest GetPasswordData where
    type Sv GetPasswordData = EC2
    type Rs GetPasswordData = GetPasswordDataResult

    request  = post "GetPasswordData"
    response = const . xmlResponse $ \h x -> GetPasswordDataResult
record
