{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatchLogs
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new log group with the specified name. The name of the log group
-- must be unique within a region for an AWS account. You can create up to 500
-- log groups per account. You must use the following guidelines when naming a
-- log group: Log group names can be between 1 and 512 characters long.
-- Allowed characters are a–z, A–Z, 0–9, '_' (underscore), '-' (hyphen), '/'
-- (forward slash), and '.' (period). Create a new Log Group The following is
-- an example of a CreateLogGroup request and response. POST / HTTP/1.1 Host:
-- logs.. X-Amz-Date: Authorization: AWS4-HMAC-SHA256 Credential=,
-- SignedHeaders=content-type;date;host;user-agent;x-amz-date;x-amz-target;x-amzn-requestid,
-- Signature= User-Agent: Accept: application/json Content-Type:
-- application/x-amz-json-1.1 Content-Length: Connection: Keep-Alive]]>
-- X-Amz-Target: Logs_20140328.CreateLogGroup { "logGroupName":
-- "exampleLogGroupName" } HTTP/1.1 200 OK x-amzn-RequestId: Content-Type:
-- application/x-amz-json-1.1 Content-Length: Date: ]]>.
module Network.AWS.CloudWatchLogs
    (
    -- * Request
      CreateLogGroup
    -- ** Request constructor
    , mkCreateLogGroup
    -- ** Request lenses
    , clgLogGroupName

    -- * Response
    , CreateLogGroupResponse
    -- ** Response constructor
    , mkCreateLogGroupResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

newtype CreateLogGroup = CreateLogGroup
    { _clgLogGroupName :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLogGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @LogGroupName ::@ @Text@
--
mkCreateLogGroup :: Text -- ^ 'clgLogGroupName'
                 -> CreateLogGroup
mkCreateLogGroup p1 = CreateLogGroup
    { _clgLogGroupName = p1
    }

clgLogGroupName :: Lens' CreateLogGroup Text
clgLogGroupName = lens _clgLogGroupName (\s a -> s { _clgLogGroupName = a })

instance ToPath CreateLogGroup

instance ToQuery CreateLogGroup

instance ToHeaders CreateLogGroup

instance ToJSON CreateLogGroup

data CreateLogGroupResponse = CreateLogGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateLogGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCreateLogGroupResponse :: CreateLogGroupResponse
mkCreateLogGroupResponse = CreateLogGroupResponse

instance AWSRequest CreateLogGroup where
    type Sv CreateLogGroup = CloudWatchLogs
    type Rs CreateLogGroup = CreateLogGroupResponse

    request = get
    response _ = nullaryResponse CreateLogGroupResponse
