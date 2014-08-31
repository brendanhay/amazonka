{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.V2014_05_15.GetOperationDetail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns the current status of an operation that is not
-- completed. GetOperationDetail Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.GetOperationDetail
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] {
-- "OperationId":"43884ce5-e30a-4801-858f-7aa86356c127" } HTTP/1.1 200
-- Content-Length:[number of characters in the JSON string] {
-- "DomainName":"happierdomain.ca",
-- "OperationId":"43884ce5-e30a-4801-858f-7aa86356c127",
-- "Status":"WORKFLOW_IN_PROGRESS", "SubmittedDate" : 1402630939.057, "Type" :
-- "REGISTER_DOMAIN" }.
module Network.AWS.Route53Domains.V2014_05_15.GetOperationDetail where

import           Network.AWS.Route53Domains.V2014_05_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

data GetOperationDetail = GetOperationDetail
    { _godrOperationId :: Text
      -- ^ The identifier for the operation for which you want to get the
      -- status. Amazon Route 53 returned the identifier in the response
      -- to the original request. Type: String Default: None Required:
      -- Yes.
    } deriving (Show, Generic)

makeLenses ''GetOperationDetail

instance ToPath GetOperationDetail

instance ToQuery GetOperationDetail

instance ToHeaders GetOperationDetail

instance ToJSON GetOperationDetail

data GetOperationDetailResponse = GetOperationDetailResponse
    { _godsDomainName :: Maybe Text
      -- ^ The name of a domain. Type: String.
    , _godsMessage :: Maybe Text
      -- ^ Detailed information on the status including possible errors.
      -- Type: String.
    , _godsOperationId :: Maybe Text
      -- ^ The identifier for the operation. Type: String.
    , _godsStatus :: Maybe OperationStatus
      -- ^ The current status of the requested operation in the system.
      -- Type: String.
    , _godsType :: Maybe OperationType
      -- ^ The type of operation that was requested. Type: String.
    , _godsSubmittedDate :: Maybe ISO8601
      -- ^ The date when the request was submitted.
    } deriving (Show, Generic)

makeLenses ''GetOperationDetailResponse

instance FromJSON GetOperationDetailResponse

instance AWSRequest GetOperationDetail where
    type Sv GetOperationDetail = Route53Domains
    type Rs GetOperationDetail = GetOperationDetailResponse

    request = get
    response _ = jsonResponse
