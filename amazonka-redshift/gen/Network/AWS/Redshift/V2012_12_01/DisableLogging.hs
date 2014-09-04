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
    , mkDisableLoggingMessage
    -- ** Request lenses
    , dlmClusterIdentifier

    -- * Response
    , DisableLoggingResponse
    -- ** Response lenses
    , lltLoggingEnabled
    , lltBucketName
    , lltS3KeyPrefix
    , lltLastSuccessfulDeliveryTime
    , lltLastFailureTime
    , lltLastFailureMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableLogging' request.
mkDisableLoggingMessage :: Text -- ^ 'dlmClusterIdentifier'
                        -> DisableLogging
mkDisableLoggingMessage p1 = DisableLogging
    { _dlmClusterIdentifier = p1
    }
{-# INLINE mkDisableLoggingMessage #-}

newtype DisableLogging = DisableLogging
    { _dlmClusterIdentifier :: Text
      -- ^ The identifier of the cluster on which logging is to be stopped.
      -- Example: examplecluster.
    } deriving (Show, Generic)

-- | The identifier of the cluster on which logging is to be stopped. Example:
-- examplecluster.
dlmClusterIdentifier :: Lens' DisableLogging (Text)
dlmClusterIdentifier = lens _dlmClusterIdentifier (\s a -> s { _dlmClusterIdentifier = a })
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
    , _lltLastSuccessfulDeliveryTime :: Maybe ISO8601
      -- ^ The last time when logs were delivered.
    , _lltLastFailureTime :: Maybe ISO8601
      -- ^ The last time when logs failed to be delivered.
    , _lltLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    } deriving (Show, Generic)

-- | true if logging is on, false if logging is off.
lltLoggingEnabled :: Lens' DisableLoggingResponse (Maybe Bool)
lltLoggingEnabled = lens _lltLoggingEnabled (\s a -> s { _lltLoggingEnabled = a })
{-# INLINE lltLoggingEnabled #-}

-- | The name of the S3 bucket where the log files are stored.
lltBucketName :: Lens' DisableLoggingResponse (Maybe Text)
lltBucketName = lens _lltBucketName (\s a -> s { _lltBucketName = a })
{-# INLINE lltBucketName #-}

-- | The prefix applied to the log file names.
lltS3KeyPrefix :: Lens' DisableLoggingResponse (Maybe Text)
lltS3KeyPrefix = lens _lltS3KeyPrefix (\s a -> s { _lltS3KeyPrefix = a })
{-# INLINE lltS3KeyPrefix #-}

-- | The last time when logs were delivered.
lltLastSuccessfulDeliveryTime :: Lens' DisableLoggingResponse (Maybe ISO8601)
lltLastSuccessfulDeliveryTime = lens _lltLastSuccessfulDeliveryTime (\s a -> s { _lltLastSuccessfulDeliveryTime = a })
{-# INLINE lltLastSuccessfulDeliveryTime #-}

-- | The last time when logs failed to be delivered.
lltLastFailureTime :: Lens' DisableLoggingResponse (Maybe ISO8601)
lltLastFailureTime = lens _lltLastFailureTime (\s a -> s { _lltLastFailureTime = a })
{-# INLINE lltLastFailureTime #-}

-- | The message indicating that logs failed to be delivered.
lltLastFailureMessage :: Lens' DisableLoggingResponse (Maybe Text)
lltLastFailureMessage = lens _lltLastFailureMessage (\s a -> s { _lltLastFailureMessage = a })
{-# INLINE lltLastFailureMessage #-}

instance FromXML DisableLoggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DisableLogging where
    type Sv DisableLogging = Redshift
    type Rs DisableLogging = DisableLoggingResponse

    request = post "DisableLogging"
    response _ = xmlResponse
