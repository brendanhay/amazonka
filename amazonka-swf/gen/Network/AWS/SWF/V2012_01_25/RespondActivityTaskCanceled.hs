{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.RespondActivityTaskCanceled
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Used by workers to tell the service that the ActivityTask identified by the
-- taskToken was successfully canceled. Additional details can be optionally
-- provided using the details argument. These details (if provided) appear in
-- the ActivityTaskCanceled event added to the workflow history. Only use this
-- operation if the canceled flag of a RecordActivityTaskHeartbeat request
-- returns true and if the activity can be safely undone or abandoned. A task
-- is considered open from the time that it is scheduled until it is closed.
-- Therefore a task is reported as open while a worker is processing it. A
-- task is closed after it has been specified in a call to
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
-- Amazon SWF Workflows. RespondActivityTaskCanceled Example POST / HTTP/1.1
-- Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0 (Windows; U;
-- Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212 Firefox/3.6.25 ( .NET
-- CLR 3.5.30729; .NET4.0E) Accept: application/json, text/javascript, */*
-- Accept-Language: en-us,en;q=0.5 Accept-Encoding: gzip,deflate
-- Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115 Connection:
-- keep-alive Content-Type: application/x-amz-json-1.0 X-Requested-With:
-- XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 04:36:44 GMT X-Amz-Target:
-- SimpleWorkflowService.RespondActivityTaskCanceled Content-Encoding: amz-1.0
-- X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=7ZMb0Np0OyXw6hrFSBFDAfBnSaEP1TH7cAG29DL5BUI=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 640 Pragma: no-cache Cache-Control: no-cache {"taskToken":
-- "AAAAKgAAAAEAAAAAAAAAAQlFok8Ay875ki85gos/Okm9kWg1Jm6DbwiBZgxyCrW2OS+DQQtrCTMr+KH1ouxrCVOkTXPOUY/M4Ujfr1CrsMi6S0DMD8/N6yxzd34+PIIvRY8w9M5z89PbPQKjKHKbz2ocbTnHgRThaBO4ZmeadNyZWSeQyZXmsQFmFuHfaH9P2ibzrDS1dU+s/iw/R9RBrRWArsph/FIfWdRUJfu/FH9IFPSb3KYKMVaJAOyWhcR1KrRGywIGxPC7m9tQjapXqitoRYj42qgABydT4NVR5cLCkeYW0LKxUGVU46+gNvRaUfYzP31JVARQh5d0j7S/ERi10m6bamPJ3UcZfLFbM42mIINywmcTORMpQ/nPGLU1iECYrtnAV0YTlGZfGm+Vi6Gcgwyi4hEjg7TCBjc6WBw3JuAfFvUPU5cfvAoX7quUZRA7JUnYGObE0y9zYuTnCx6C1GL7Ks2MEA0coIiAl4JZx6qsGYfeKjIGntTsoCEe1zjp5gRqfeD74kfeZg0HmqA0xiFGZ40OHbImnF5YHsedYfLk6u09SAkQMD8iJhT8",
-- "details": "customer canceled transaction"} HTTP/1.1 200 OK Content-Length:
-- 0 Content-Type: application/json x-amzn-RequestId:
-- b1a001a6-3ffb-11e1-9b11-7182192d0b57.
module Network.AWS.SWF.V2012_01_25.RespondActivityTaskCanceled
    (
    -- * Request
      RespondActivityTaskCanceled
    -- ** Request constructor
    , mkRespondActivityTaskCanceled
    -- ** Request lenses
    , ratcTaskToken
    , ratcDetails

    -- * Response
    , RespondActivityTaskCanceledResponse
    ) where

import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data RespondActivityTaskCanceled = RespondActivityTaskCanceled
    { _ratcTaskToken :: Text
    , _ratcDetails :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'RespondActivityTaskCanceled' request.
mkRespondActivityTaskCanceled :: Text -- ^ 'ratcTaskToken'
                              -> RespondActivityTaskCanceled
mkRespondActivityTaskCanceled p1 = RespondActivityTaskCanceled
    { _ratcTaskToken = p1
    , _ratcDetails = Nothing
    }

-- | The taskToken of the ActivityTask. The taskToken is generated by the
-- service and should be treated as an opaque value. If the task is passed to
-- another process, its taskToken must also be passed. This enables it to
-- provide its progress and respond with results.
ratcTaskToken :: Lens' RespondActivityTaskCanceled Text
ratcTaskToken = lens _ratcTaskToken (\s a -> s { _ratcTaskToken = a })

-- | Optional information about the cancellation.
ratcDetails :: Lens' RespondActivityTaskCanceled (Maybe Text)
ratcDetails = lens _ratcDetails (\s a -> s { _ratcDetails = a })

instance ToPath RespondActivityTaskCanceled

instance ToQuery RespondActivityTaskCanceled

instance ToHeaders RespondActivityTaskCanceled

instance ToJSON RespondActivityTaskCanceled

data RespondActivityTaskCanceledResponse = RespondActivityTaskCanceledResponse
    deriving (Eq, Show, Generic)

instance AWSRequest RespondActivityTaskCanceled where
    type Sv RespondActivityTaskCanceled = SWF
    type Rs RespondActivityTaskCanceled = RespondActivityTaskCanceledResponse

    request = get
    response _ = nullaryResponse RespondActivityTaskCanceledResponse
