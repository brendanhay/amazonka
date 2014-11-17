{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DescribeLoggingStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes whether information, such as queries and connection attempts, is
-- being logged for the specified Amazon Redshift cluster.
--
-- <http://docs.aws.amazon.com/redshift/latest/APIReference/API_DescribeLoggingStatus.html>
module Network.AWS.Redshift.DescribeLoggingStatus
    (
    -- * Request
      DescribeLoggingStatus
    -- ** Request constructor
    , describeLoggingStatus
    -- ** Request lenses
    , dlsClusterIdentifier

    -- * Response
    , DescribeLoggingStatusResponse
    -- ** Response constructor
    , describeLoggingStatusResponse
    -- ** Response lenses
    , dlsrBucketName
    , dlsrLastFailureMessage
    , dlsrLastFailureTime
    , dlsrLastSuccessfulDeliveryTime
    , dlsrLoggingEnabled
    , dlsrS3KeyPrefix
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import qualified GHC.Exts

newtype DescribeLoggingStatus = DescribeLoggingStatus
    { _dlsClusterIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DescribeLoggingStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlsClusterIdentifier' @::@ 'Text'
--
describeLoggingStatus :: Text -- ^ 'dlsClusterIdentifier'
                      -> DescribeLoggingStatus
describeLoggingStatus p1 = DescribeLoggingStatus
    { _dlsClusterIdentifier = p1
    }

-- | The identifier of the cluster to get the logging status from. Example:
-- examplecluster.
dlsClusterIdentifier :: Lens' DescribeLoggingStatus Text
dlsClusterIdentifier =
    lens _dlsClusterIdentifier (\s a -> s { _dlsClusterIdentifier = a })

data DescribeLoggingStatusResponse = DescribeLoggingStatusResponse
    { _dlsrBucketName                 :: Maybe Text
    , _dlsrLastFailureMessage         :: Maybe Text
    , _dlsrLastFailureTime            :: Maybe RFC822
    , _dlsrLastSuccessfulDeliveryTime :: Maybe RFC822
    , _dlsrLoggingEnabled             :: Maybe Bool
    , _dlsrS3KeyPrefix                :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeLoggingStatusResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlsrBucketName' @::@ 'Maybe' 'Text'
--
-- * 'dlsrLastFailureMessage' @::@ 'Maybe' 'Text'
--
-- * 'dlsrLastFailureTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dlsrLastSuccessfulDeliveryTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'dlsrLoggingEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'dlsrS3KeyPrefix' @::@ 'Maybe' 'Text'
--
describeLoggingStatusResponse :: DescribeLoggingStatusResponse
describeLoggingStatusResponse = DescribeLoggingStatusResponse
    { _dlsrLoggingEnabled             = Nothing
    , _dlsrBucketName                 = Nothing
    , _dlsrS3KeyPrefix                = Nothing
    , _dlsrLastSuccessfulDeliveryTime = Nothing
    , _dlsrLastFailureTime            = Nothing
    , _dlsrLastFailureMessage         = Nothing
    }

-- | The name of the S3 bucket where the log files are stored.
dlsrBucketName :: Lens' DescribeLoggingStatusResponse (Maybe Text)
dlsrBucketName = lens _dlsrBucketName (\s a -> s { _dlsrBucketName = a })

-- | The message indicating that logs failed to be delivered.
dlsrLastFailureMessage :: Lens' DescribeLoggingStatusResponse (Maybe Text)
dlsrLastFailureMessage =
    lens _dlsrLastFailureMessage (\s a -> s { _dlsrLastFailureMessage = a })

-- | The last time when logs failed to be delivered.
dlsrLastFailureTime :: Lens' DescribeLoggingStatusResponse (Maybe UTCTime)
dlsrLastFailureTime =
    lens _dlsrLastFailureTime (\s a -> s { _dlsrLastFailureTime = a })
        . mapping _Time

-- | The last time when logs were delivered.
dlsrLastSuccessfulDeliveryTime :: Lens' DescribeLoggingStatusResponse (Maybe UTCTime)
dlsrLastSuccessfulDeliveryTime =
    lens _dlsrLastSuccessfulDeliveryTime
        (\s a -> s { _dlsrLastSuccessfulDeliveryTime = a })
            . mapping _Time

-- | true if logging is on, false if logging is off.
dlsrLoggingEnabled :: Lens' DescribeLoggingStatusResponse (Maybe Bool)
dlsrLoggingEnabled =
    lens _dlsrLoggingEnabled (\s a -> s { _dlsrLoggingEnabled = a })

-- | The prefix applied to the log file names.
dlsrS3KeyPrefix :: Lens' DescribeLoggingStatusResponse (Maybe Text)
dlsrS3KeyPrefix = lens _dlsrS3KeyPrefix (\s a -> s { _dlsrS3KeyPrefix = a })

instance ToPath DescribeLoggingStatus where
    toPath = const "/"

instance ToQuery DescribeLoggingStatus

instance ToHeaders DescribeLoggingStatus

instance AWSRequest DescribeLoggingStatus where
    type Sv DescribeLoggingStatus = Redshift
    type Rs DescribeLoggingStatus = DescribeLoggingStatusResponse

    request  = post "DescribeLoggingStatus"
    response = xmlResponse

instance FromXML DescribeLoggingStatusResponse where
    parseXML c = DescribeLoggingStatusResponse
        <$> c .: "BucketName"
        <*> c .: "LastFailureMessage"
        <*> c .: "LastFailureTime"
        <*> c .: "LastSuccessfulDeliveryTime"
        <*> c .: "LoggingEnabled"
        <*> c .: "S3KeyPrefix"
