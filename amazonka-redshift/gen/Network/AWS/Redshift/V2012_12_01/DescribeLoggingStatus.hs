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
    -- ** Response lenses
    , dlsrsLoggingEnabled
    , dlsrsBucketName
    , dlsrsS3KeyPrefix
    , dlsrsLastSuccessfulDeliveryTime
    , dlsrsLastFailureTime
    , dlsrsLastFailureMessage
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
mkDescribeLoggingStatus :: Text -- ^ 'dlsClusterIdentifier'
                        -> DescribeLoggingStatus
mkDescribeLoggingStatus p1 = DescribeLoggingStatus
    { _dlsClusterIdentifier = p1
    }
{-# INLINE mkDescribeLoggingStatus #-}

-- | The identifier of the cluster to get the logging status from. Example:
-- examplecluster.
dlsClusterIdentifier :: Lens' DescribeLoggingStatus Text
dlsClusterIdentifier =
    lens _dlsClusterIdentifier (\s a -> s { _dlsClusterIdentifier = a })
{-# INLINE dlsClusterIdentifier #-}

instance ToQuery DescribeLoggingStatus where
    toQuery = genericQuery def

-- | Describes the status of logging for a cluster.
data DescribeLoggingStatusResponse = DescribeLoggingStatusResponse
    { _dlsrsLoggingEnabled :: Maybe Bool
    , _dlsrsBucketName :: Maybe Text
    , _dlsrsS3KeyPrefix :: Maybe Text
    , _dlsrsLastSuccessfulDeliveryTime :: Maybe ISO8601
    , _dlsrsLastFailureTime :: Maybe ISO8601
    , _dlsrsLastFailureMessage :: Maybe Text
    } deriving (Show, Generic)

-- | true if logging is on, false if logging is off.
dlsrsLoggingEnabled :: Lens' DescribeLoggingStatusResponse (Maybe Bool)
dlsrsLoggingEnabled =
    lens _dlsrsLoggingEnabled (\s a -> s { _dlsrsLoggingEnabled = a })
{-# INLINE dlsrsLoggingEnabled #-}

-- | The name of the S3 bucket where the log files are stored.
dlsrsBucketName :: Lens' DescribeLoggingStatusResponse (Maybe Text)
dlsrsBucketName = lens _dlsrsBucketName (\s a -> s { _dlsrsBucketName = a })
{-# INLINE dlsrsBucketName #-}

-- | The prefix applied to the log file names.
dlsrsS3KeyPrefix :: Lens' DescribeLoggingStatusResponse (Maybe Text)
dlsrsS3KeyPrefix =
    lens _dlsrsS3KeyPrefix (\s a -> s { _dlsrsS3KeyPrefix = a })
{-# INLINE dlsrsS3KeyPrefix #-}

-- | The last time when logs were delivered.
dlsrsLastSuccessfulDeliveryTime :: Lens' DescribeLoggingStatusResponse (Maybe ISO8601)
dlsrsLastSuccessfulDeliveryTime =
    lens _dlsrsLastSuccessfulDeliveryTime
         (\s a -> s { _dlsrsLastSuccessfulDeliveryTime = a })
{-# INLINE dlsrsLastSuccessfulDeliveryTime #-}

-- | The last time when logs failed to be delivered.
dlsrsLastFailureTime :: Lens' DescribeLoggingStatusResponse (Maybe ISO8601)
dlsrsLastFailureTime =
    lens _dlsrsLastFailureTime (\s a -> s { _dlsrsLastFailureTime = a })
{-# INLINE dlsrsLastFailureTime #-}

-- | The message indicating that logs failed to be delivered.
dlsrsLastFailureMessage :: Lens' DescribeLoggingStatusResponse (Maybe Text)
dlsrsLastFailureMessage =
    lens _dlsrsLastFailureMessage
         (\s a -> s { _dlsrsLastFailureMessage = a })
{-# INLINE dlsrsLastFailureMessage #-}

instance FromXML DescribeLoggingStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoggingStatus where
    type Sv DescribeLoggingStatus = Redshift
    type Rs DescribeLoggingStatus = DescribeLoggingStatusResponse

    request = post "DescribeLoggingStatus"
    response _ = xmlResponse
