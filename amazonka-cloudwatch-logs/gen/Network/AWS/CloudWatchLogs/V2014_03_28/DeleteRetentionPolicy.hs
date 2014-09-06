{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs.V2014_03_28.DeleteRetentionPolicy
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the retention policy of the specified log group. Log events would
-- not expire if they belong to log groups without a retention policy. Deletes
-- the retention policy of a log group The following is an example of a
-- DeleteRetentionPolicy request and response. POST / HTTP/1.1 Host: logs..
-- X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.DeleteRetentionPolicy { "logGroupName":
-- "exampleLogGroupName" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]>.
module Network.AWS.CloudWatchLogs.V2014_03_28.DeleteRetentionPolicy
    (
    -- * Request
      DeleteRetentionPolicy
    -- ** Request constructor
    , mkDeleteRetentionPolicy
    -- ** Request lenses
    , drpLogGroupName

    -- * Response
    , DeleteRetentionPolicyResponse
    ) where

import           Network.AWS.CloudWatchLogs.V2014_03_28.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

newtype DeleteRetentionPolicy = DeleteRetentionPolicy
    { _drpLogGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteRetentionPolicy' request.
mkDeleteRetentionPolicy :: Text -- ^ 'drpLogGroupName'
                        -> DeleteRetentionPolicy
mkDeleteRetentionPolicy p1 = DeleteRetentionPolicy
    { _drpLogGroupName = p1
    }
{-# INLINE mkDeleteRetentionPolicy #-}

drpLogGroupName :: Lens' DeleteRetentionPolicy Text
drpLogGroupName = lens _drpLogGroupName (\s a -> s { _drpLogGroupName = a })
{-# INLINE drpLogGroupName #-}

instance ToPath DeleteRetentionPolicy

instance ToQuery DeleteRetentionPolicy

instance ToHeaders DeleteRetentionPolicy

instance ToJSON DeleteRetentionPolicy

data DeleteRetentionPolicyResponse = DeleteRetentionPolicyResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteRetentionPolicy where
    type Sv DeleteRetentionPolicy = CloudWatchLogs
    type Rs DeleteRetentionPolicy = DeleteRetentionPolicyResponse

    request = get
    response _ = nullaryResponse DeleteRetentionPolicyResponse
