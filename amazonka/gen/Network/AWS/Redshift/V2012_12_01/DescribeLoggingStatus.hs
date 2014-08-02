{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.Redshift.V2012_12_01.DescribeLoggingStatus where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data DescribeLoggingStatus = DescribeLoggingStatus
    { _dlsmClusterIdentifier :: Text
      -- ^ The identifier of the cluster to get the logging status from.
      -- Example: examplecluster.
    } deriving (Generic)

makeLenses ''DescribeLoggingStatus

instance ToQuery DescribeLoggingStatus where
    toQuery = genericToQuery def

data DescribeLoggingStatusResponse = DescribeLoggingStatusResponse
    { _llzLoggingEnabled :: Maybe Bool
      -- ^ true if logging is on, false if logging is off.
    , _llzS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names.
    , _llzBucketName :: Maybe Text
      -- ^ The name of the S3 bucket where the log files are stored.
    , _llzLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    , _llzLastFailureTime :: Maybe ISO8601
      -- ^ The last time when logs failed to be delivered.
    , _llzLastSuccessfulDeliveryTime :: Maybe ISO8601
      -- ^ The last time when logs were delivered.
    } deriving (Generic)

makeLenses ''DescribeLoggingStatusResponse

instance FromXML DescribeLoggingStatusResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeLoggingStatus where
    type Sv DescribeLoggingStatus = Redshift
    type Rs DescribeLoggingStatus = DescribeLoggingStatusResponse

    request = post "DescribeLoggingStatus"
    response _ = xmlResponse
