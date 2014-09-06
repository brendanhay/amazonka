{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.RespondActivityTaskFailed
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Used by workers to tell the service that the ActivityTask identified by the
-- taskToken has failed with reason (if specified). The reason and details
-- appear in the ActivityTaskFailed event added to the workflow history. A
-- task is considered open from the time that it is scheduled until it is
-- closed. Therefore a task is reported as open while a worker is processing
-- it. A task is closed after it has been specified in a call to
-- RespondActivityTaskCompleted, RespondActivityTaskCanceled,
-- RespondActivityTaskFailed, or the task has timed out. Access Control You
-- can use IAM policies to control this action's access to Amazon SWF
-- resources as follows: Use a Resource element with the domain name to limit
-- the action to only specified domains. Use an Action element to allow or
-- deny permission to call this action. You cannot use an IAM policy to
-- constrain this action's parameters. If the caller does not have sufficient
-- permissions to invoke the action, or the parameter values fall outside the
-- specified constraints, the action fails by throwing OperationNotPermitted.
-- For details and example IAM policies, see Using IAM to Manage Access to
-- Amazon SWF Workflows. RespondActivityTaskFailed Example POST / HTTP/1.1
-- Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U;
-- Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET
-- CLR 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 04:17:24 GMT X-Amz-Target:
-- SimpleWorkflowService.RespondActivityTaskFailed Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=JC+/uds/mFEq8qca2WFs5kfp2eAEONc70IqFgHErhpc=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 682 Pragma: no-cache Cache-Control: no-cache {"taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAdG7j7YFEl9pfKdXRL3Cy3Q3c1Z8QwdOSX53bKiUV6MMGXvf3Lrinmmzj1HFFl5lcwHzEFxLbMaSZ/lMt/RFJPumHXAnUqlYjZLODhrBqsIzDQFKcbCFMq7y4jm0EFzsV2Suv8iu/obcZ/idU8qjd9uG/82zumG2xz1Z4IbOFwOTlpj2++5YVH4ftyycIcjlDw58r0O1vAo4PEondkqjyn+YxBxyZLy1z1fvMi0zeO8Lh16w96y6v+KdVc/ECoez1Og8sROaXG0l8ptW5YR733LIuUBK4sxWa12egF5i4e8AV8JloojOaq0jy4iFsIscRazOSQErjo15Guz89BK2XW911P3I+X7nJjH0wwW55XGCs0jezvsEC8M6D9Ob7CgWr6RrnK3g1AKemcby2XqgQRN52DMIYxzV+lMS/QBYKOqtkLoMY0NKeuRVwm9f1zCY00v6kxqK9m2zFvaxqlJ5/JVCWMNWEWJfQZVtC3GzMWmzeCt7Auq8A5/Caq/DKyOhTIhY/Go00iiDA6ecP8taTYiVzb8VR5xEiQ1uCxnECkwW",
-- "reason": "could not verify customer credit card", "details": "card number
-- invalid"} HTTP/1.1 200 OK Content-Length: 0 Content-Type: application/json
-- x-amzn-RequestId: feadaedd-3ff8-11e1-9e8f-57bb03e21482.
module Network.AWS.SWF.V2012_01_25.RespondActivityTaskFailed
    (
    -- * Request
      RespondActivityTaskFailed
    -- ** Request constructor
    , mkRespondActivityTaskFailed
    -- ** Request lenses
    , ratfTaskToken
    , ratfReason
    , ratfDetails

    -- * Response
    , RespondActivityTaskFailedResponse
    ) where

import           Network.AWS.SWF.V2012_01_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data RespondActivityTaskFailed = RespondActivityTaskFailed
    { _ratfTaskToken :: Text
    , _ratfReason :: Maybe Text
    , _ratfDetails :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RespondActivityTaskFailed' request.
mkRespondActivityTaskFailed :: Text -- ^ 'ratfTaskToken'
                            -> RespondActivityTaskFailed
mkRespondActivityTaskFailed p1 = RespondActivityTaskFailed
    { _ratfTaskToken = p1
    , _ratfReason = Nothing
    , _ratfDetails = Nothing
    }
{-# INLINE mkRespondActivityTaskFailed #-}

-- | The taskToken of the ActivityTask. The taskToken is generated by the
-- service and should be treated as an opaque value. If the task is passed to
-- another process, its taskToken must also be passed. This enables it to
-- provide its progress and respond with results.
ratfTaskToken :: Lens' RespondActivityTaskFailed Text
ratfTaskToken = lens _ratfTaskToken (\s a -> s { _ratfTaskToken = a })
{-# INLINE ratfTaskToken #-}

-- | Description of the error that may assist in diagnostics.
ratfReason :: Lens' RespondActivityTaskFailed (Maybe Text)
ratfReason = lens _ratfReason (\s a -> s { _ratfReason = a })
{-# INLINE ratfReason #-}

-- | Optional detailed information about the failure.
ratfDetails :: Lens' RespondActivityTaskFailed (Maybe Text)
ratfDetails = lens _ratfDetails (\s a -> s { _ratfDetails = a })
{-# INLINE ratfDetails #-}

instance ToPath RespondActivityTaskFailed

instance ToQuery RespondActivityTaskFailed

instance ToHeaders RespondActivityTaskFailed

instance ToJSON RespondActivityTaskFailed

data RespondActivityTaskFailedResponse = RespondActivityTaskFailedResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RespondActivityTaskFailed where
    type Sv RespondActivityTaskFailed = SWF
    type Rs RespondActivityTaskFailed = RespondActivityTaskFailedResponse

    request = get
    response _ = nullaryResponse RespondActivityTaskFailedResponse
