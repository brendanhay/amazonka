{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.DeleteMetricFilter
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a metric filter associated with the specified log group. Delete a
-- metric filter The following is an example of a DeleteMetricFilter request
-- and response. POST / HTTP/1.1 Host: logs.. X-Amz-Date: Authorization:
-- AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DeleteMetricFilter { "logGroupName":
-- "exampleLogGroupName", "filterName": "exampleMetricFilterName" } HTTP/1.1
-- 200 OK x-amzn-RequestId: Content-Type: application/x-amz-json-1.1
-- Content-Length: Date: ]]>.
module Network.AWS.CloudWatchLogs
    (
    -- * Request
      DeleteMetricFilter
    -- ** Request constructor
    , mkDeleteMetricFilter
    -- ** Request lenses
    , dmfLogGroupName
    , dmfFilterName

    -- * Response
    , DeleteMetricFilterResponse
    -- ** Response constructor
    , mkDeleteMetricFilterResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DeleteMetricFilter = DeleteMetricFilter
    { _dmfLogGroupName :: !Text
    , _dmfFilterName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteMetricFilter' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LogGroupName ::@ @Text@
--
-- * @FilterName ::@ @Text@
--
mkDeleteMetricFilter :: Text -- ^ 'dmfLogGroupName'
                     -> Text -- ^ 'dmfFilterName'
                     -> DeleteMetricFilter
mkDeleteMetricFilter p1 p2 = DeleteMetricFilter
    { _dmfLogGroupName = p1
    , _dmfFilterName = p2
    }

dmfLogGroupName :: Lens' DeleteMetricFilter Text
dmfLogGroupName = lens _dmfLogGroupName (\s a -> s { _dmfLogGroupName = a })

-- | The name of the metric filter.
dmfFilterName :: Lens' DeleteMetricFilter Text
dmfFilterName = lens _dmfFilterName (\s a -> s { _dmfFilterName = a })

instance ToPath DeleteMetricFilter

instance ToQuery DeleteMetricFilter

instance ToHeaders DeleteMetricFilter

instance ToJSON DeleteMetricFilter

data DeleteMetricFilterResponse = DeleteMetricFilterResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteMetricFilterResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteMetricFilterResponse :: DeleteMetricFilterResponse
mkDeleteMetricFilterResponse = DeleteMetricFilterResponse

instance AWSRequest DeleteMetricFilter where
    type Sv DeleteMetricFilter = CloudWatchLogs
    type Rs DeleteMetricFilter = DeleteMetricFilterResponse

    request = get
    response _ = nullaryResponse DeleteMetricFilterResponse
