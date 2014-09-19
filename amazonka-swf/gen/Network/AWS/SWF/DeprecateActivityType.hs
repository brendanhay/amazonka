{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.DeprecateActivityType
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deprecates the specified activity type. After an activity type has been
-- deprecated, you cannot create new tasks of that activity type. Tasks of
-- this type that were scheduled before the type was deprecated will continue
-- to run. This operation is eventually consistent. The results are best
-- effort and may not exactly reflect recent updates and changes. Access
-- Control You can use IAM policies to control this action's access to Amazon
-- SWF resources as follows: Use a Resource element with the domain name to
-- limit the action to only specified domains. Use an Action element to allow
-- or deny permission to call this action. Constrain the following parameters
-- by using a Condition element with the appropriate keys. activityType.name:
-- String constraint. The key is swf:activityType.name. activityType.version:
-- String constraint. The key is swf:activityType.version. If the caller does
-- not have sufficient permissions to invoke the action, or the parameter
-- values fall outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. DeprecateActivityType Example
-- POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent: Mozilla/5.0
-- (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Mon, 16 Jan 2012 05:01:06 GMT
-- X-Amz-Target: SimpleWorkflowService.DeprecateActivityType Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=iX/mNMtNH6IaSNwfZq9hHOhDlLnp7buuj9tO93kRIrQ=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 95 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "activityType": {"name": "activityVerify", "version": "1.0"} }
-- HTTP/1.1 200 OK Content-Length: 0 Content-Type: application/json
-- x-amzn-RequestId: 191ee17e-3fff-11e1-a23a-99d60383ae71.
module Network.AWS.SWF.DeprecateActivityType
    (
    -- * Request
      DeprecateActivityType
    -- ** Request constructor
    , deprecateActivityType
    -- ** Request lenses
    , datDomain
    , datActivityType

    -- * Response
    , DeprecateActivityTypeResponse
    -- ** Response constructor
    , deprecateActivityTypeResponse
    ) where

import Network.AWS.SWF.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data DeprecateActivityType = DeprecateActivityType
    { _datDomain :: Text
    , _datActivityType :: ActivityType
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeprecateActivityType' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Domain ::@ @Text@
--
-- * @ActivityType ::@ @ActivityType@
--
deprecateActivityType :: Text -- ^ 'datDomain'
                      -> ActivityType -- ^ 'datActivityType'
                      -> DeprecateActivityType
deprecateActivityType p1 p2 = DeprecateActivityType
    { _datDomain = p1
    , _datActivityType = p2
    }

-- | The name of the domain in which the activity type is registered.
datDomain :: Lens' DeprecateActivityType Text
datDomain = lens _datDomain (\s a -> s { _datDomain = a })

-- | The activity type to deprecate.
datActivityType :: Lens' DeprecateActivityType ActivityType
datActivityType = lens _datActivityType (\s a -> s { _datActivityType = a })

instance ToPath DeprecateActivityType

instance ToQuery DeprecateActivityType

instance ToHeaders DeprecateActivityType

instance ToJSON DeprecateActivityType

data DeprecateActivityTypeResponse = DeprecateActivityTypeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeprecateActivityTypeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deprecateActivityTypeResponse :: DeprecateActivityTypeResponse
deprecateActivityTypeResponse = DeprecateActivityTypeResponse

instance AWSRequest DeprecateActivityType where
    type Sv DeprecateActivityType = SWF
    type Rs DeprecateActivityType = DeprecateActivityTypeResponse

    request = get
    response _ = nullaryResponse DeprecateActivityTypeResponse
