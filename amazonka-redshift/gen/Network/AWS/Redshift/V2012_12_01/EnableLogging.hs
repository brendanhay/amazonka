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
    , mkEnableLogging
    -- ** Request lenses
    , elClusterIdentifier
    , elBucketName
    , elS3KeyPrefix

    -- * Response
    , EnableLoggingResponse
    -- ** Response lenses
    , elrsLoggingEnabled
    , elrsBucketName
    , elrsS3KeyPrefix
    , elrsLastSuccessfulDeliveryTime
    , elrsLastFailureTime
    , elrsLastFailureMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
data EnableLogging = EnableLogging
    { _elClusterIdentifier :: Text
    , _elBucketName :: Text
    , _elS3KeyPrefix :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnableLogging' request.
mkEnableLogging :: Text -- ^ 'elClusterIdentifier'
                -> Text -- ^ 'elBucketName'
                -> EnableLogging
mkEnableLogging p1 p2 = EnableLogging
    { _elClusterIdentifier = p1
    , _elBucketName = p2
    , _elS3KeyPrefix = Nothing
    }

-- | The identifier of the cluster on which logging is to be started. Example:
-- examplecluster.
elClusterIdentifier :: Lens' EnableLogging Text
elClusterIdentifier =
    lens _elClusterIdentifier (\s a -> s { _elClusterIdentifier = a })

-- | The name of an existing S3 bucket where the log files are to be stored.
-- Constraints: Must be in the same region as the cluster The cluster must
-- have read bucket and put object permissions.
elBucketName :: Lens' EnableLogging Text
elBucketName = lens _elBucketName (\s a -> s { _elBucketName = a })

-- | The prefix applied to the log file names. Constraints: Cannot exceed 512
-- characters Cannot contain spaces( ), double quotes ("), single quotes ('),
-- a backslash (\), or control characters. The hexadecimal codes for invalid
-- characters are: x00 to x20 x22 x27 x5c x7f or larger.
elS3KeyPrefix :: Lens' EnableLogging (Maybe Text)
elS3KeyPrefix = lens _elS3KeyPrefix (\s a -> s { _elS3KeyPrefix = a })

instance ToQuery EnableLogging where
    toQuery = genericQuery def

-- | Describes the status of logging for a cluster.
data EnableLoggingResponse = EnableLoggingResponse
    { _elrsLoggingEnabled :: Maybe Bool
    , _elrsBucketName :: Maybe Text
    , _elrsS3KeyPrefix :: Maybe Text
    , _elrsLastSuccessfulDeliveryTime :: Maybe ISO8601
    , _elrsLastFailureTime :: Maybe ISO8601
    , _elrsLastFailureMessage :: Maybe Text
    } deriving (Show, Generic)

-- | true if logging is on, false if logging is off.
elrsLoggingEnabled :: Lens' EnableLoggingResponse (Maybe Bool)
elrsLoggingEnabled =
    lens _elrsLoggingEnabled (\s a -> s { _elrsLoggingEnabled = a })

-- | The name of the S3 bucket where the log files are stored.
elrsBucketName :: Lens' EnableLoggingResponse (Maybe Text)
elrsBucketName = lens _elrsBucketName (\s a -> s { _elrsBucketName = a })

-- | The prefix applied to the log file names.
elrsS3KeyPrefix :: Lens' EnableLoggingResponse (Maybe Text)
elrsS3KeyPrefix = lens _elrsS3KeyPrefix (\s a -> s { _elrsS3KeyPrefix = a })

-- | The last time when logs were delivered.
elrsLastSuccessfulDeliveryTime :: Lens' EnableLoggingResponse (Maybe ISO8601)
elrsLastSuccessfulDeliveryTime =
    lens _elrsLastSuccessfulDeliveryTime
         (\s a -> s { _elrsLastSuccessfulDeliveryTime = a })

-- | The last time when logs failed to be delivered.
elrsLastFailureTime :: Lens' EnableLoggingResponse (Maybe ISO8601)
elrsLastFailureTime =
    lens _elrsLastFailureTime (\s a -> s { _elrsLastFailureTime = a })

-- | The message indicating that logs failed to be delivered.
elrsLastFailureMessage :: Lens' EnableLoggingResponse (Maybe Text)
elrsLastFailureMessage =
    lens _elrsLastFailureMessage (\s a -> s { _elrsLastFailureMessage = a })

instance FromXML EnableLoggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest EnableLogging where
    type Sv EnableLogging = Redshift
    type Rs EnableLogging = EnableLoggingResponse

    request = post "EnableLogging"
    response _ = xmlResponse
