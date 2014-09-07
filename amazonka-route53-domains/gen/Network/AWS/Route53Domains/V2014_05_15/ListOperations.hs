{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53Domains.V2014_05_15.ListOperations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | This operation returns the operation IDs of operations that are not yet
-- complete. ListOperations Example POST / HTTP/1.1
-- host:route53domains.us-east-1.amazonaws.com x-amz-date:20140711T205230Z
-- authorization:AWS4-HMAC-SHA256
-- Credential=AKIAIOSFODNN7EXAMPLE/20140711/us-east-1/route53domains/aws4_request,
-- 
-- SignedHeaders=content-length;content-type;host;user-agent;x-amz-date;x-amz-target,
-- Signature=[calculated-signature]
-- x-amz-target:Route53Domains_v20140515.ListOperations
-- user-agent:aws-sdk-java/1.8.3 Linux/2.6.18-164.el5PAE Java_HotSpot (TM
-- )_Server_VM/24.60-b09/1.7.0_60 content-type:application/x-amz-json-1.1
-- content-length:[number of characters in the JSON string] { "MaxItems" : 2 }
-- HTTP/1.1 200 Content-Length:[number of characters in the JSON string] {
-- "Operations":[ { "OperationId":"4ced3d4a-e011-45ee-b94f-1e2d73477562",
-- "Status":"WORKFLOW_IN_PROGRESS", "SubmittedDate":1403548979.088,
-- "Type":"CHANGE_PRIVACY_PROTECTION" }, {
-- "OperationId":"2e3ac45b-89b3-47ea-a042-f56dcd1b6883",
-- "Status":"WORKFLOW_IN_PROGRESS", "SubmittedDate":1403548986.429,
-- "Type":"DOMAIN_LOCK" } ] }.
module Network.AWS.Route53Domains.V2014_05_15.ListOperations
    (
    -- * Request
      ListOperations
    -- ** Request constructor
    , mkListOperations
    -- ** Request lenses
    , loMarker
    , loMaxItems

    -- * Response
    , ListOperationsResponse
    -- ** Response lenses
    , lorsOperations
    , lorsNextPageMarker
    ) where

import Network.AWS.Route53Domains.V2014_05_15.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The ListOperations request includes the following elements.
data ListOperations = ListOperations
    { _loMarker :: Maybe Text
    , _loMaxItems :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListOperations' request.
mkListOperations :: ListOperations
mkListOperations = ListOperations
    { _loMarker = Nothing
    , _loMaxItems = Nothing
    }

-- | For an initial request for a list of operations, omit this element. If the
-- number of operations that are not yet complete is greater than the value
-- that you specified for MaxItems, you can use Marker to return additional
-- operations. Get the value of NextPageMarker from the previous response, and
-- submit another request that includes the value of NextPageMarker in the
-- Marker element. Type: String Default: None Required: No.
loMarker :: Lens' ListOperations (Maybe Text)
loMarker = lens _loMarker (\s a -> s { _loMarker = a })

-- | Number of domains to be returned. Type: Integer Default: 20 Constraints: A
-- value between 1 and 100. Required: No.
loMaxItems :: Lens' ListOperations (Maybe Integer)
loMaxItems = lens _loMaxItems (\s a -> s { _loMaxItems = a })

instance ToPath ListOperations

instance ToQuery ListOperations

instance ToHeaders ListOperations

instance ToJSON ListOperations

-- | The ListOperations response includes the following elements.
data ListOperationsResponse = ListOperationsResponse
    { _lorsOperations :: [OperationSummary]
    , _lorsNextPageMarker :: Maybe Text
    } deriving (Show, Generic)

-- | Lists summaries of the operations. Type: Complex type containing a list of
-- operation summaries Children: OperationId, Status, SubmittedDate, Type.
lorsOperations :: Lens' ListOperationsResponse [OperationSummary]
lorsOperations = lens _lorsOperations (\s a -> s { _lorsOperations = a })

-- | If there are more operations than you specified for MaxItems in the
-- request, submit another request and include the value of NextPageMarker in
-- the value of Marker. Type: String Parent: Operations.
lorsNextPageMarker :: Lens' ListOperationsResponse (Maybe Text)
lorsNextPageMarker =
    lens _lorsNextPageMarker (\s a -> s { _lorsNextPageMarker = a })

instance FromJSON ListOperationsResponse

instance AWSRequest ListOperations where
    type Sv ListOperations = Route53Domains
    type Rs ListOperations = ListOperationsResponse

    request = get
    response _ = jsonResponse
