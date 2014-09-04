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
    , describeLoggingStatus
    -- ** Request lenses
    , dlsmClusterIdentifier

    -- * Response
    , DescribeLoggingStatusResponse
    -- ** Response lenses
    , llsLoggingEnabled
    , llsBucketName
    , llsS3KeyPrefix
    , llsLastFailureMessage
    , llsLastSuccessfulDeliveryTime
    , llsLastFailureTime
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeLoggingStatus' request.
describeLoggingStatus :: Text -- ^ 'dlsmClusterIdentifier'
                      -> DescribeLoggingStatus
describeLoggingStatus p1 = DescribeLoggingStatus
    { _dlsmClusterIdentifier = p1
    }
{-# INLINE describeLoggingStatus #-}

data DescribeLoggingStatus = DescribeLoggingStatus
    { _dlsmClusterIdentifier :: Text
      -- ^ The identifier of the cluster to get the logging status from.
      -- Example: examplecluster.
    } deriving (Show, Generic)

-- | The identifier of the cluster to get the logging status from. Example:
-- examplecluster.
dlsmClusterIdentifier :: Lens' DescribeLoggingStatus (Text)
dlsmClusterIdentifier f x =
    f (_dlsmClusterIdentifier x)
        <&> \y -> x { _dlsmClusterIdentifier = y }
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
    , _llsLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    , _llsLastSuccessfulDeliveryTime :: Maybe ISO8601
      -- ^ The last time when logs were delivered.
    , _llsLastFailureTime :: Maybe ISO8601
      -- ^ The last time when logs failed to be delivered.
    } deriving (Show, Generic)

-- | true if logging is on, false if logging is off.
llsLoggingEnabled :: Lens' DescribeLoggingStatusResponse (Maybe Bool)
llsLoggingEnabled f x =
    f (_llsLoggingEnabled x)
        <&> \y -> x { _llsLoggingEnabled = y }
{-# INLINE llsLoggingEnabled #-}

-- | The name of the S3 bucket where the log files are stored.
llsBucketName :: Lens' DescribeLoggingStatusResponse (Maybe Text)
llsBucketName f x =
    f (_llsBucketName x)
        <&> \y -> x { _llsBucketName = y }
{-# INLINE llsBucketName #-}

-- | The prefix applied to the log file names.
llsS3KeyPrefix :: Lens' DescribeLoggingStatusResponse (Maybe Text)
llsS3KeyPrefix f x =
    f (_llsS3KeyPrefix x)
        <&> \y -> x { _llsS3KeyPrefix = y }
{-# INLINE llsS3KeyPrefix #-}

-- | The message indicating that logs failed to be delivered.
llsLastFailureMessage :: Lens' DescribeLoggingStatusResponse (Maybe Text)
llsLastFailureMessage f x =
    f (_llsLastFailureMessage x)
        <&> \y -> x { _llsLastFailureMessage = y }
{-# INLINE llsLastFailureMessage #-}

-- | The last time when logs were delivered.
llsLastSuccessfulDeliveryTime :: Lens' DescribeLoggingStatusResponse (Maybe ISO8601)
llsLastSuccessfulDeliveryTime f x =
    f (_llsLastSuccessfulDeliveryTime x)
        <&> \y -> x { _llsLastSuccessfulDeliveryTime = y }
{-# INLINE llsLastSuccessfulDeliveryTime #-}

-- | The last time when logs failed to be delivered.
llsLastFailureTime :: Lens' DescribeLoggingStatusResponse (Maybe ISO8601)
llsLastFailureTime f x =
    f (_llsLastFailureTime x)
        <&> \y -> x { _llsLastFailureTime = y }
{-# INLINE llsLastFailureTime #-}

instance FromXML DescribeLoggingStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoggingStatus where
    type Sv DescribeLoggingStatus = Redshift
    type Rs DescribeLoggingStatus = DescribeLoggingStatusResponse

    request = post "DescribeLoggingStatus"
    response _ = xmlResponse
