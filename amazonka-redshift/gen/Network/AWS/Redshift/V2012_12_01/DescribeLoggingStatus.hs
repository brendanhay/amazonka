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
    , mkDescribeLoggingStatusMessage
    -- ** Request lenses
    , dlsmClusterIdentifier

    -- * Response
    , DescribeLoggingStatusResponse
    -- ** Response lenses
    , llsLoggingEnabled
    , llsBucketName
    , llsS3KeyPrefix
    , llsLastSuccessfulDeliveryTime
    , llsLastFailureTime
    , llsLastFailureMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeLoggingStatus' request.
mkDescribeLoggingStatusMessage :: Text -- ^ 'dlsmClusterIdentifier'
                               -> DescribeLoggingStatus
mkDescribeLoggingStatusMessage p1 = DescribeLoggingStatus
    { _dlsmClusterIdentifier = p1
    }
{-# INLINE mkDescribeLoggingStatusMessage #-}

newtype DescribeLoggingStatus = DescribeLoggingStatus
    { _dlsmClusterIdentifier :: Text
      -- ^ The identifier of the cluster to get the logging status from.
      -- Example: examplecluster.
    } deriving (Show, Generic)

-- | The identifier of the cluster to get the logging status from. Example:
-- examplecluster.
dlsmClusterIdentifier :: Lens' DescribeLoggingStatus (Text)
dlsmClusterIdentifier = lens _dlsmClusterIdentifier (\s a -> s { _dlsmClusterIdentifier = a })
{-# INLINE dlsmClusterIdentifier #-}

instance ToQuery DescribeLoggingStatus where
    toQuery = genericQuery def

data DescribeLoggingStatusResponse = DescribeLoggingStatusResponse
    { _llsLoggingEnabled :: Maybe Bool
      -- ^ true if logging is on, false if logging is off.
    , _llsBucketName :: Maybe Text
      -- ^ The name of the S3 bucket where the log files are stored.
    , _llsS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names.
    , _llsLastSuccessfulDeliveryTime :: Maybe ISO8601
      -- ^ The last time when logs were delivered.
    , _llsLastFailureTime :: Maybe ISO8601
      -- ^ The last time when logs failed to be delivered.
    , _llsLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    } deriving (Show, Generic)

-- | true if logging is on, false if logging is off.
llsLoggingEnabled :: Lens' DescribeLoggingStatusResponse (Maybe Bool)
llsLoggingEnabled = lens _llsLoggingEnabled (\s a -> s { _llsLoggingEnabled = a })
{-# INLINE llsLoggingEnabled #-}

-- | The name of the S3 bucket where the log files are stored.
llsBucketName :: Lens' DescribeLoggingStatusResponse (Maybe Text)
llsBucketName = lens _llsBucketName (\s a -> s { _llsBucketName = a })
{-# INLINE llsBucketName #-}

-- | The prefix applied to the log file names.
llsS3KeyPrefix :: Lens' DescribeLoggingStatusResponse (Maybe Text)
llsS3KeyPrefix = lens _llsS3KeyPrefix (\s a -> s { _llsS3KeyPrefix = a })
{-# INLINE llsS3KeyPrefix #-}

-- | The last time when logs were delivered.
llsLastSuccessfulDeliveryTime :: Lens' DescribeLoggingStatusResponse (Maybe ISO8601)
llsLastSuccessfulDeliveryTime = lens _llsLastSuccessfulDeliveryTime (\s a -> s { _llsLastSuccessfulDeliveryTime = a })
{-# INLINE llsLastSuccessfulDeliveryTime #-}

-- | The last time when logs failed to be delivered.
llsLastFailureTime :: Lens' DescribeLoggingStatusResponse (Maybe ISO8601)
llsLastFailureTime = lens _llsLastFailureTime (\s a -> s { _llsLastFailureTime = a })
{-# INLINE llsLastFailureTime #-}

-- | The message indicating that logs failed to be delivered.
llsLastFailureMessage :: Lens' DescribeLoggingStatusResponse (Maybe Text)
llsLastFailureMessage = lens _llsLastFailureMessage (\s a -> s { _llsLastFailureMessage = a })
{-# INLINE llsLastFailureMessage #-}

instance FromXML DescribeLoggingStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoggingStatus where
    type Sv DescribeLoggingStatus = Redshift
    type Rs DescribeLoggingStatus = DescribeLoggingStatusResponse

    request = post "DescribeLoggingStatus"
    response _ = xmlResponse
