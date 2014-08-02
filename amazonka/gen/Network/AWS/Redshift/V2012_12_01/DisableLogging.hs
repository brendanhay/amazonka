{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.Redshift.V2012_12_01.DisableLogging where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

data DisableLogging = DisableLogging
    { _dlmClusterIdentifier :: Text
      -- ^ The identifier of the cluster on which logging is to be stopped.
      -- Example: examplecluster.
    } deriving (Generic)

makeLenses ''DisableLogging

instance ToQuery DisableLogging where
    toQuery = genericToQuery def

data DisableLoggingResponse = DisableLoggingResponse
    { _lsLoggingEnabled :: Maybe Bool
      -- ^ true if logging is on, false if logging is off.
    , _lsS3KeyPrefix :: Maybe Text
      -- ^ The prefix applied to the log file names.
    , _lsBucketName :: Maybe Text
      -- ^ The name of the S3 bucket where the log files are stored.
    , _lsLastFailureMessage :: Maybe Text
      -- ^ The message indicating that logs failed to be delivered.
    , _lsLastFailureTime :: Maybe ISO8601
      -- ^ The last time when logs failed to be delivered.
    , _lsLastSuccessfulDeliveryTime :: Maybe ISO8601
      -- ^ The last time when logs were delivered.
    } deriving (Generic)

makeLenses ''DisableLoggingResponse

instance FromXML DisableLoggingResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DisableLogging where
    type Sv DisableLogging = Redshift
    type Rs DisableLogging = DisableLoggingResponse

    request = post "DisableLogging"
    response _ = xmlResponse
