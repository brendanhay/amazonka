{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.Route53Domains.V2014_05_15.GetOperationDetail
    (
    -- * Request
      GetOperationDetail
    -- ** Request constructor
    , mkGetOperationDetail
    -- ** Request lenses
    , godOperationId

    -- * Response
    , GetOperationDetailResponse
    -- ** Response lenses
    , godrsOperationId
    , godrsStatus
    , godrsMessage
    , godrsDomainName
    , godrsType
    , godrsSubmittedDate
    ) where

import           Network.AWS.Route53Domains.V2014_05_15.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | The GetOperationDetail request includes the following element.
newtype GetOperationDetail = GetOperationDetail
    { _godOperationId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetOperationDetail' request.
mkGetOperationDetail :: Text -- ^ 'godOperationId'
                     -> GetOperationDetail
mkGetOperationDetail p1 = GetOperationDetail
    { _godOperationId = p1
    }
{-# INLINE mkGetOperationDetail #-}

-- | The identifier for the operation for which you want to get the status.
-- Amazon Route 53 returned the identifier in the response to the original
-- request. Type: String Default: None Required: Yes.
godOperationId :: Lens' GetOperationDetail Text
godOperationId = lens _godOperationId (\s a -> s { _godOperationId = a })
{-# INLINE godOperationId #-}

instance ToPath GetOperationDetail

instance ToQuery GetOperationDetail

instance ToHeaders GetOperationDetail

instance ToJSON GetOperationDetail

-- | The GetOperationDetail response includes the following elements.
data GetOperationDetailResponse = GetOperationDetailResponse
    { _godrsOperationId :: Maybe Text
    , _godrsStatus :: Maybe OperationStatus
    , _godrsMessage :: Maybe Text
    , _godrsDomainName :: Maybe Text
    , _godrsType :: Maybe OperationType
    , _godrsSubmittedDate :: Maybe ISO8601
    } deriving (Show, Generic)

-- | The identifier for the operation. Type: String.
godrsOperationId :: Lens' GetOperationDetailResponse (Maybe Text)
godrsOperationId =
    lens _godrsOperationId (\s a -> s { _godrsOperationId = a })
{-# INLINE godrsOperationId #-}

-- | The current status of the requested operation in the system. Type: String.
godrsStatus :: Lens' GetOperationDetailResponse (Maybe OperationStatus)
godrsStatus = lens _godrsStatus (\s a -> s { _godrsStatus = a })
{-# INLINE godrsStatus #-}

-- | Detailed information on the status including possible errors. Type: String.
godrsMessage :: Lens' GetOperationDetailResponse (Maybe Text)
godrsMessage = lens _godrsMessage (\s a -> s { _godrsMessage = a })
{-# INLINE godrsMessage #-}

-- | The name of a domain. Type: String.
godrsDomainName :: Lens' GetOperationDetailResponse (Maybe Text)
godrsDomainName = lens _godrsDomainName (\s a -> s { _godrsDomainName = a })
{-# INLINE godrsDomainName #-}

-- | The type of operation that was requested. Type: String.
godrsType :: Lens' GetOperationDetailResponse (Maybe OperationType)
godrsType = lens _godrsType (\s a -> s { _godrsType = a })
{-# INLINE godrsType #-}

-- | The date when the request was submitted.
godrsSubmittedDate :: Lens' GetOperationDetailResponse (Maybe ISO8601)
godrsSubmittedDate =
    lens _godrsSubmittedDate (\s a -> s { _godrsSubmittedDate = a })
{-# INLINE godrsSubmittedDate #-}

instance FromJSON GetOperationDetailResponse

instance AWSRequest GetOperationDetail where
    type Sv GetOperationDetail = Route53Domains
    type Rs GetOperationDetail = GetOperationDetailResponse

    request = get
    response _ = jsonResponse
