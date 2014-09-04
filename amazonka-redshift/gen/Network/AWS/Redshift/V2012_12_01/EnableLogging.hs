{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.EnableLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts logging information, such as queries and connection attempts, for
-- the specified Amazon Redshift cluster.
module Network.AWS.Redshift.V2012_12_01.EnableLogging
    (
    -- * Request
      EnableLogging
    -- ** Request constructor
    , mkEnableLoggingMessage
    -- ** Request lenses
    , elmClusterIdentifier
    , elmBucketName
    , elmS3KeyPrefix

    -- * Response
    , EnableLoggingResponse
    -- ** Response lenses
    , lluLoggingEnabled
    , lluBucketName
    , lluS3KeyPrefix
    , lluLastSuccessfulDeliveryTime
    , lluLastFailureTime
    , lluLastFailureMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnableLogging' request.
mkEnableLoggingMessage :: Text -- ^ 'elmClusterIdentifier'
                       -> Text -- ^ 'elmBucketName'
                       -> EnableLogging
mkEnableLoggingMessage p1 p2 = EnableLogging
    { _elmClusterIdentifier = p1
    , _elmBucketName = p2
    , _elmS3KeyPrefix = Nothing
    }
{-# INLINE mkEnableLoggingMessage #-}

data EnableLogging = EnableLogging
    { _elmClusterIdentifier :: Text
      -- ^ The identifier of the cluster on which logging is to be started.
      -- Example: examplecluster.
    , _elmBucketName :: Text
      -- ^ The name of an existing S3 bucket where the log files are to be
      -- stored. Constraints: Must be in the same region as the cluster
      -- The cluster must have read bucket and put object permissions.
    , _elmS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names. Constraints: Cannot
      -- exceed 512 characters Cannot contain spaces( ), double quotes
      -- ("), single quotes ('), a backslash (\), or control characters.
      -- The hexadecimal codes for invalid characters are: x00 to x20 x22
      -- x27 x5c x7f or larger.
    } deriving (Show, Generic)

-- | The identifier of the cluster on which logging is to be started. Example:
-- examplecluster.
elmClusterIdentifier :: Lens' EnableLogging (Text)
elmClusterIdentifier = lens _elmClusterIdentifier (\s a -> s { _elmClusterIdentifier = a })
{-# INLINE elmClusterIdentifier #-}

-- | The name of an existing S3 bucket where the log files are to be stored.
-- Constraints: Must be in the same region as the cluster The cluster must
-- have read bucket and put object permissions.
elmBucketName :: Lens' EnableLogging (Text)
elmBucketName = lens _elmBucketName (\s a -> s { _elmBucketName = a })
{-# INLINE elmBucketName #-}

-- | The prefix applied to the log file names. Constraints: Cannot exceed 512
-- characters Cannot contain spaces( ), double quotes ("), single quotes ('),
-- a backslash (\), or control characters. The hexadecimal codes for invalid
-- characters are: x00 to x20 x22 x27 x5c x7f or larger.
elmS3KeyPrefix :: Lens' EnableLogging (Maybe Text)
elmS3KeyPrefix = lens _elmS3KeyPrefix (\s a -> s { _elmS3KeyPrefix = a })
{-# INLINE elmS3KeyPrefix #-}

instance ToQuery EnableLogging where
    toQuery = genericQuery def

data EnableLoggingResponse = EnableLoggingResponse
    { _lluLoggingEnabled :: Maybe Bool
      -- ^ true if logging is on, false if logging is off.
    , _lluBucketName :: Maybe Text
      -- ^ The name of the S3 bucket where the log files are stored.
    , _lluS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names.
    , _lluLastSuccessfulDeliveryTime :: Maybe ISO8601
      -- ^ The last time when logs were delivered.
    , _lluLastFailureTime :: Maybe ISO8601
      -- ^ The last time when logs failed to be delivered.
    , _lluLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    } deriving (Show, Generic)

-- | true if logging is on, false if logging is off.
lluLoggingEnabled :: Lens' EnableLoggingResponse (Maybe Bool)
lluLoggingEnabled = lens _lluLoggingEnabled (\s a -> s { _lluLoggingEnabled = a })
{-# INLINE lluLoggingEnabled #-}

-- | The name of the S3 bucket where the log files are stored.
lluBucketName :: Lens' EnableLoggingResponse (Maybe Text)
lluBucketName = lens _lluBucketName (\s a -> s { _lluBucketName = a })
{-# INLINE lluBucketName #-}

-- | The prefix applied to the log file names.
lluS3KeyPrefix :: Lens' EnableLoggingResponse (Maybe Text)
lluS3KeyPrefix = lens _lluS3KeyPrefix (\s a -> s { _lluS3KeyPrefix = a })
{-# INLINE lluS3KeyPrefix #-}

-- | The last time when logs were delivered.
lluLastSuccessfulDeliveryTime :: Lens' EnableLoggingResponse (Maybe ISO8601)
lluLastSuccessfulDeliveryTime = lens _lluLastSuccessfulDeliveryTime (\s a -> s { _lluLastSuccessfulDeliveryTime = a })
{-# INLINE lluLastSuccessfulDeliveryTime #-}

-- | The last time when logs failed to be delivered.
lluLastFailureTime :: Lens' EnableLoggingResponse (Maybe ISO8601)
lluLastFailureTime = lens _lluLastFailureTime (\s a -> s { _lluLastFailureTime = a })
{-# INLINE lluLastFailureTime #-}

-- | The message indicating that logs failed to be delivered.
lluLastFailureMessage :: Lens' EnableLoggingResponse (Maybe Text)
lluLastFailureMessage = lens _lluLastFailureMessage (\s a -> s { _lluLastFailureMessage = a })
{-# INLINE lluLastFailureMessage #-}

instance FromXML EnableLoggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest EnableLogging where
    type Sv EnableLogging = Redshift
    type Rs EnableLogging = EnableLoggingResponse

    request = post "EnableLogging"
    response _ = xmlResponse
