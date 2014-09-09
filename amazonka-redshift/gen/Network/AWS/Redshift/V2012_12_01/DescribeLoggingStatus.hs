{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DescribeLoggingStatus
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
module Network.AWS.Redshift.V2012_12_01.DescribeLoggingStatus
    (
    -- * Request
      DescribeLoggingStatus
    -- ** Request constructor
    , mkDescribeLoggingStatus
    -- ** Request lenses
    , dlsClusterIdentifier

    -- * Response
    , DescribeLoggingStatusResponse
    -- ** Response constructor
    , mkDescribeLoggingStatusResponse
    -- ** Response lenses
    , dlsrLoggingEnabled
    , dlsrBucketName
    , dlsrS3KeyPrefix
    , dlsrLastSuccessfulDeliveryTime
    , dlsrLastFailureTime
    , dlsrLastFailureMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
newtype DescribeLoggingStatus = DescribeLoggingStatus
    { _dlsClusterIdentifier :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoggingStatus' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterIdentifier ::@ @Text@
--
mkDescribeLoggingStatus :: Text -- ^ 'dlsClusterIdentifier'
                        -> DescribeLoggingStatus
mkDescribeLoggingStatus p1 = DescribeLoggingStatus
    { _dlsClusterIdentifier = p1
    }

-- | The identifier of the cluster to get the logging status from. Example:
-- examplecluster.
dlsClusterIdentifier :: Lens' DescribeLoggingStatus Text
dlsClusterIdentifier =
    lens _dlsClusterIdentifier (\s a -> s { _dlsClusterIdentifier = a })

instance ToQuery DescribeLoggingStatus where
    toQuery = genericQuery def

-- | Describes the status of logging for a cluster.
data DescribeLoggingStatusResponse = DescribeLoggingStatusResponse
    { _dlsrLoggingEnabled :: Maybe Bool
    , _dlsrBucketName :: Maybe Text
    , _dlsrS3KeyPrefix :: Maybe Text
    , _dlsrLastSuccessfulDeliveryTime :: Maybe ISO8601
    , _dlsrLastFailureTime :: Maybe ISO8601
    , _dlsrLastFailureMessage :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoggingStatusResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LoggingEnabled ::@ @Maybe Bool@
--
-- * @BucketName ::@ @Maybe Text@
--
-- * @S3KeyPrefix ::@ @Maybe Text@
--
-- * @LastSuccessfulDeliveryTime ::@ @Maybe ISO8601@
--
-- * @LastFailureTime ::@ @Maybe ISO8601@
--
-- * @LastFailureMessage ::@ @Maybe Text@
--
mkDescribeLoggingStatusResponse :: DescribeLoggingStatusResponse
mkDescribeLoggingStatusResponse = DescribeLoggingStatusResponse
    { _dlsrLoggingEnabled = Nothing
    , _dlsrBucketName = Nothing
    , _dlsrS3KeyPrefix = Nothing
    , _dlsrLastSuccessfulDeliveryTime = Nothing
    , _dlsrLastFailureTime = Nothing
    , _dlsrLastFailureMessage = Nothing
    }

-- | true if logging is on, false if logging is off.
dlsrLoggingEnabled :: Lens' DescribeLoggingStatusResponse (Maybe Bool)
dlsrLoggingEnabled =
    lens _dlsrLoggingEnabled (\s a -> s { _dlsrLoggingEnabled = a })

-- | The name of the S3 bucket where the log files are stored.
dlsrBucketName :: Lens' DescribeLoggingStatusResponse (Maybe Text)
dlsrBucketName = lens _dlsrBucketName (\s a -> s { _dlsrBucketName = a })

-- | The prefix applied to the log file names.
dlsrS3KeyPrefix :: Lens' DescribeLoggingStatusResponse (Maybe Text)
dlsrS3KeyPrefix = lens _dlsrS3KeyPrefix (\s a -> s { _dlsrS3KeyPrefix = a })

-- | The last time when logs were delivered.
dlsrLastSuccessfulDeliveryTime :: Lens' DescribeLoggingStatusResponse (Maybe ISO8601)
dlsrLastSuccessfulDeliveryTime =
    lens _dlsrLastSuccessfulDeliveryTime
         (\s a -> s { _dlsrLastSuccessfulDeliveryTime = a })

-- | The last time when logs failed to be delivered.
dlsrLastFailureTime :: Lens' DescribeLoggingStatusResponse (Maybe ISO8601)
dlsrLastFailureTime =
    lens _dlsrLastFailureTime (\s a -> s { _dlsrLastFailureTime = a })

-- | The message indicating that logs failed to be delivered.
dlsrLastFailureMessage :: Lens' DescribeLoggingStatusResponse (Maybe Text)
dlsrLastFailureMessage =
    lens _dlsrLastFailureMessage (\s a -> s { _dlsrLastFailureMessage = a })

instance FromXML DescribeLoggingStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoggingStatus where
    type Sv DescribeLoggingStatus = Redshift
    type Rs DescribeLoggingStatus = DescribeLoggingStatusResponse

    request = post "DescribeLoggingStatus"
    response _ = xmlResponse
