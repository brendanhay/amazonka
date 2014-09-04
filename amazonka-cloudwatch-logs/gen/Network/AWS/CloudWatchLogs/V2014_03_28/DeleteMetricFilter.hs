{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.DeleteMetricFilter
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
module Network.AWS.CloudWatchLogs.V2014_03_28.DeleteMetricFilter
    (
    -- * Request
      DeleteMetricFilter
    -- ** Request constructor
    , mkDeleteMetricFilterRequest
    -- ** Request lenses
    , dmfrLogGroupName
    , dmfrFilterName

    -- * Response
    , DeleteMetricFilterResponse
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteMetricFilter' request.
mkDeleteMetricFilterRequest :: Text -- ^ 'dmfrLogGroupName'
                            -> Text -- ^ 'dmfrFilterName'
                            -> DeleteMetricFilter
mkDeleteMetricFilterRequest p1 p2 = DeleteMetricFilter
    { _dmfrLogGroupName = p1
    , _dmfrFilterName = p2
    }
{-# INLINE mkDeleteMetricFilterRequest #-}

data DeleteMetricFilter = DeleteMetricFilter
    { _dmfrLogGroupName :: Text
    , _dmfrFilterName :: Text
      -- ^ The name of the metric filter.
    } deriving (Show, Generic)

dmfrLogGroupName :: Lens' DeleteMetricFilter (Text)
dmfrLogGroupName = lens _dmfrLogGroupName (\s a -> s { _dmfrLogGroupName = a })
{-# INLINE dmfrLogGroupName #-}

-- | The name of the metric filter.
dmfrFilterName :: Lens' DeleteMetricFilter (Text)
dmfrFilterName = lens _dmfrFilterName (\s a -> s { _dmfrFilterName = a })
{-# INLINE dmfrFilterName #-}

instance ToPath DeleteMetricFilter

instance ToQuery DeleteMetricFilter

instance ToHeaders DeleteMetricFilter

instance ToJSON DeleteMetricFilter

data DeleteMetricFilterResponse = DeleteMetricFilterResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteMetricFilter where
    type Sv DeleteMetricFilter = CloudWatchLogs
    type Rs DeleteMetricFilter = DeleteMetricFilterResponse

    request = get
    response _ = nullaryResponse DeleteMetricFilterResponse
