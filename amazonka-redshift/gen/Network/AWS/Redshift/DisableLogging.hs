{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.DisableLogging
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
module Network.AWS.Redshift.DisableLogging
    (
    -- * Request
      DisableLogging
    -- ** Request constructor
    , disableLogging
    -- ** Request lenses
    , dlClusterIdentifier

    -- * Response
    , DisableLoggingResponse
    -- ** Response constructor
    , disableLoggingResponse
    -- ** Response lenses
    , dlrLoggingEnabled
    , dlrBucketName
    , dlrS3KeyPrefix
    , dlrLastSuccessfulDeliveryTime
    , dlrLastFailureTime
    , dlrLastFailureMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
newtype DisableLogging = DisableLogging
    { _dlClusterIdentifier :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableLogging' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterIdentifier ::@ @Text@
--
disableLogging :: Text -- ^ 'dlClusterIdentifier'
               -> DisableLogging
disableLogging p1 = DisableLogging
    { _dlClusterIdentifier = p1
    }

-- | The identifier of the cluster on which logging is to be stopped. Example:
-- examplecluster.
dlClusterIdentifier :: Lens' DisableLogging Text
dlClusterIdentifier =
    lens _dlClusterIdentifier (\s a -> s { _dlClusterIdentifier = a })

instance ToQuery DisableLogging where
    toQuery = genericQuery def

-- | Describes the status of logging for a cluster.
data DisableLoggingResponse = DisableLoggingResponse
    { _dlrLoggingEnabled :: Maybe Bool
    , _dlrBucketName :: Maybe Text
    , _dlrS3KeyPrefix :: Maybe Text
    , _dlrLastSuccessfulDeliveryTime :: Maybe ISO8601
    , _dlrLastFailureTime :: Maybe ISO8601
    , _dlrLastFailureMessage :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableLoggingResponse' response.
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
disableLoggingResponse :: DisableLoggingResponse
disableLoggingResponse = DisableLoggingResponse
    { _dlrLoggingEnabled = Nothing
    , _dlrBucketName = Nothing
    , _dlrS3KeyPrefix = Nothing
    , _dlrLastSuccessfulDeliveryTime = Nothing
    , _dlrLastFailureTime = Nothing
    , _dlrLastFailureMessage = Nothing
    }

-- | true if logging is on, false if logging is off.
dlrLoggingEnabled :: Lens' DisableLoggingResponse (Maybe Bool)
dlrLoggingEnabled =
    lens _dlrLoggingEnabled (\s a -> s { _dlrLoggingEnabled = a })

-- | The name of the S3 bucket where the log files are stored.
dlrBucketName :: Lens' DisableLoggingResponse (Maybe Text)
dlrBucketName = lens _dlrBucketName (\s a -> s { _dlrBucketName = a })

-- | The prefix applied to the log file names.
dlrS3KeyPrefix :: Lens' DisableLoggingResponse (Maybe Text)
dlrS3KeyPrefix = lens _dlrS3KeyPrefix (\s a -> s { _dlrS3KeyPrefix = a })

-- | The last time when logs were delivered.
dlrLastSuccessfulDeliveryTime :: Lens' DisableLoggingResponse (Maybe ISO8601)
dlrLastSuccessfulDeliveryTime =
    lens _dlrLastSuccessfulDeliveryTime
         (\s a -> s { _dlrLastSuccessfulDeliveryTime = a })

-- | The last time when logs failed to be delivered.
dlrLastFailureTime :: Lens' DisableLoggingResponse (Maybe ISO8601)
dlrLastFailureTime =
    lens _dlrLastFailureTime (\s a -> s { _dlrLastFailureTime = a })

-- | The message indicating that logs failed to be delivered.
dlrLastFailureMessage :: Lens' DisableLoggingResponse (Maybe Text)
dlrLastFailureMessage =
    lens _dlrLastFailureMessage (\s a -> s { _dlrLastFailureMessage = a })

instance FromXML DisableLoggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DisableLogging where
    type Sv DisableLogging = Redshift
    type Rs DisableLogging = DisableLoggingResponse

    request = post "DisableLogging"
    response _ = xmlResponse
