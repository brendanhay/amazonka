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
    , mkDisableLogging
    -- ** Request lenses
    , dlClusterIdentifier

    -- * Response
    , DisableLoggingResponse
    -- ** Response lenses
    , dlrsLoggingEnabled
    , dlrsBucketName
    , dlrsS3KeyPrefix
    , dlrsLastSuccessfulDeliveryTime
    , dlrsLastFailureTime
    , dlrsLastFailureMessage
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | 
newtype DisableLogging = DisableLogging
    { _dlClusterIdentifier :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableLogging' request.
mkDisableLogging :: Text -- ^ 'dlClusterIdentifier'
                 -> DisableLogging
mkDisableLogging p1 = DisableLogging
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
    { _dlrsLoggingEnabled :: Maybe Bool
    , _dlrsBucketName :: Maybe Text
    , _dlrsS3KeyPrefix :: Maybe Text
    , _dlrsLastSuccessfulDeliveryTime :: Maybe ISO8601
    , _dlrsLastFailureTime :: Maybe ISO8601
    , _dlrsLastFailureMessage :: Maybe Text
    } deriving (Show, Generic)

-- | true if logging is on, false if logging is off.
dlrsLoggingEnabled :: Lens' DisableLoggingResponse (Maybe Bool)
dlrsLoggingEnabled =
    lens _dlrsLoggingEnabled (\s a -> s { _dlrsLoggingEnabled = a })

-- | The name of the S3 bucket where the log files are stored.
dlrsBucketName :: Lens' DisableLoggingResponse (Maybe Text)
dlrsBucketName = lens _dlrsBucketName (\s a -> s { _dlrsBucketName = a })

-- | The prefix applied to the log file names.
dlrsS3KeyPrefix :: Lens' DisableLoggingResponse (Maybe Text)
dlrsS3KeyPrefix = lens _dlrsS3KeyPrefix (\s a -> s { _dlrsS3KeyPrefix = a })

-- | The last time when logs were delivered.
dlrsLastSuccessfulDeliveryTime :: Lens' DisableLoggingResponse (Maybe ISO8601)
dlrsLastSuccessfulDeliveryTime =
    lens _dlrsLastSuccessfulDeliveryTime
         (\s a -> s { _dlrsLastSuccessfulDeliveryTime = a })

-- | The last time when logs failed to be delivered.
dlrsLastFailureTime :: Lens' DisableLoggingResponse (Maybe ISO8601)
dlrsLastFailureTime =
    lens _dlrsLastFailureTime (\s a -> s { _dlrsLastFailureTime = a })

-- | The message indicating that logs failed to be delivered.
dlrsLastFailureMessage :: Lens' DisableLoggingResponse (Maybe Text)
dlrsLastFailureMessage =
    lens _dlrsLastFailureMessage (\s a -> s { _dlrsLastFailureMessage = a })

instance FromXML DisableLoggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DisableLogging where
    type Sv DisableLogging = Redshift
    type Rs DisableLogging = DisableLoggingResponse

    request = post "DisableLogging"
    response _ = xmlResponse
