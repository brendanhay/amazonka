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

data DescribeLoggingStatus = DescribeLoggingStatus
    { _dlsmClusterIdentifier :: Text
      -- ^ The identifier of the cluster to get the logging status from.
      -- Example: examplecluster.
    } deriving (Show, Generic)

-- | The identifier of the cluster to get the logging status from. Example:
-- examplecluster.
dlsmClusterIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeLoggingStatus
    -> f DescribeLoggingStatus
dlsmClusterIdentifier f x =
    (\y -> x { _dlsmClusterIdentifier = y })
       <$> f (_dlsmClusterIdentifier x)
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
llsLoggingEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DescribeLoggingStatusResponse
    -> f DescribeLoggingStatusResponse
llsLoggingEnabled f x =
    (\y -> x { _llsLoggingEnabled = y })
       <$> f (_llsLoggingEnabled x)
{-# INLINE llsLoggingEnabled #-}

-- | The name of the S3 bucket where the log files are stored.
llsBucketName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeLoggingStatusResponse
    -> f DescribeLoggingStatusResponse
llsBucketName f x =
    (\y -> x { _llsBucketName = y })
       <$> f (_llsBucketName x)
{-# INLINE llsBucketName #-}

-- | The prefix applied to the log file names.
llsS3KeyPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeLoggingStatusResponse
    -> f DescribeLoggingStatusResponse
llsS3KeyPrefix f x =
    (\y -> x { _llsS3KeyPrefix = y })
       <$> f (_llsS3KeyPrefix x)
{-# INLINE llsS3KeyPrefix #-}

-- | The message indicating that logs failed to be delivered.
llsLastFailureMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeLoggingStatusResponse
    -> f DescribeLoggingStatusResponse
llsLastFailureMessage f x =
    (\y -> x { _llsLastFailureMessage = y })
       <$> f (_llsLastFailureMessage x)
{-# INLINE llsLastFailureMessage #-}

-- | The last time when logs were delivered.
llsLastSuccessfulDeliveryTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DescribeLoggingStatusResponse
    -> f DescribeLoggingStatusResponse
llsLastSuccessfulDeliveryTime f x =
    (\y -> x { _llsLastSuccessfulDeliveryTime = y })
       <$> f (_llsLastSuccessfulDeliveryTime x)
{-# INLINE llsLastSuccessfulDeliveryTime #-}

-- | The last time when logs failed to be delivered.
llsLastFailureTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DescribeLoggingStatusResponse
    -> f DescribeLoggingStatusResponse
llsLastFailureTime f x =
    (\y -> x { _llsLastFailureTime = y })
       <$> f (_llsLastFailureTime x)
{-# INLINE llsLastFailureTime #-}

instance FromXML DescribeLoggingStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoggingStatus where
    type Sv DescribeLoggingStatus = Redshift
    type Rs DescribeLoggingStatus = DescribeLoggingStatusResponse

    request = post "DescribeLoggingStatus"
    response _ = xmlResponse
