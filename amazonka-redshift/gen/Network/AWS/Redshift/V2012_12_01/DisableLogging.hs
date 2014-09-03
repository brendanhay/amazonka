{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.DisableLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Stops logging information, such as queries and connection attempts, for the
-- specified Amazon Redshift cluster.
module Network.AWS.Redshift.V2012_12_01.DisableLogging
    (
    -- * Request
      DisableLogging
    -- ** Request constructor
    , disableLogging
    -- ** Request lenses
    , dlmClusterIdentifier

    -- * Response
    , DisableLoggingResponse
    -- ** Response lenses
    , lltLoggingEnabled
    , lltBucketName
    , lltS3KeyPrefix
    , lltLastFailureMessage
    , lltLastSuccessfulDeliveryTime
    , lltLastFailureTime
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DisableLogging' request.
disableLogging :: Text -- ^ 'dlmClusterIdentifier'
               -> DisableLogging
disableLogging p1 = DisableLogging
    { _dlmClusterIdentifier = p1
    }

data DisableLogging = DisableLogging
    { _dlmClusterIdentifier :: Text
      -- ^ The identifier of the cluster on which logging is to be stopped.
      -- Example: examplecluster.
    } deriving (Show, Generic)

-- | The identifier of the cluster on which logging is to be stopped. Example:
-- examplecluster.
dlmClusterIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> DisableLogging
    -> f DisableLogging
dlmClusterIdentifier f x =
    (\y -> x { _dlmClusterIdentifier = y })
       <$> f (_dlmClusterIdentifier x)
{-# INLINE dlmClusterIdentifier #-}

instance ToQuery DisableLogging where
    toQuery = genericQuery def

data DisableLoggingResponse = DisableLoggingResponse
    { _lltLoggingEnabled :: Maybe Bool
      -- ^ true if logging is on, false if logging is off.
    , _lltBucketName :: Maybe Text
      -- ^ The name of the S3 bucket where the log files are stored.
    , _lltS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names.
    , _lltLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    , _lltLastSuccessfulDeliveryTime :: Maybe ISO8601
      -- ^ The last time when logs were delivered.
    , _lltLastFailureTime :: Maybe ISO8601
      -- ^ The last time when logs failed to be delivered.
    } deriving (Show, Generic)

-- | true if logging is on, false if logging is off.
lltLoggingEnabled
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DisableLoggingResponse
    -> f DisableLoggingResponse
lltLoggingEnabled f x =
    (\y -> x { _lltLoggingEnabled = y })
       <$> f (_lltLoggingEnabled x)
{-# INLINE lltLoggingEnabled #-}

-- | The name of the S3 bucket where the log files are stored.
lltBucketName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DisableLoggingResponse
    -> f DisableLoggingResponse
lltBucketName f x =
    (\y -> x { _lltBucketName = y })
       <$> f (_lltBucketName x)
{-# INLINE lltBucketName #-}

-- | The prefix applied to the log file names.
lltS3KeyPrefix
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DisableLoggingResponse
    -> f DisableLoggingResponse
lltS3KeyPrefix f x =
    (\y -> x { _lltS3KeyPrefix = y })
       <$> f (_lltS3KeyPrefix x)
{-# INLINE lltS3KeyPrefix #-}

-- | The message indicating that logs failed to be delivered.
lltLastFailureMessage
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DisableLoggingResponse
    -> f DisableLoggingResponse
lltLastFailureMessage f x =
    (\y -> x { _lltLastFailureMessage = y })
       <$> f (_lltLastFailureMessage x)
{-# INLINE lltLastFailureMessage #-}

-- | The last time when logs were delivered.
lltLastSuccessfulDeliveryTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DisableLoggingResponse
    -> f DisableLoggingResponse
lltLastSuccessfulDeliveryTime f x =
    (\y -> x { _lltLastSuccessfulDeliveryTime = y })
       <$> f (_lltLastSuccessfulDeliveryTime x)
{-# INLINE lltLastSuccessfulDeliveryTime #-}

-- | The last time when logs failed to be delivered.
lltLastFailureTime
    :: Functor f
    => (Maybe ISO8601
    -> f (Maybe ISO8601))
    -> DisableLoggingResponse
    -> f DisableLoggingResponse
lltLastFailureTime f x =
    (\y -> x { _lltLastFailureTime = y })
       <$> f (_lltLastFailureTime x)
{-# INLINE lltLastFailureTime #-}

instance FromXML DisableLoggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DisableLogging where
    type Sv DisableLogging = Redshift
    type Rs DisableLogging = DisableLoggingResponse

    request = post "DisableLogging"
    response _ = xmlResponse
