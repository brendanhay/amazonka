{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SWF.V2012_01_25.ListWorkflowTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about workflow types in the specified domain. The
-- results may be split into multiple pages that can be retrieved by making
-- the call repeatedly. Access Control You can use IAM policies to control
-- this action's access to Amazon SWF resources as follows: Use a Resource
-- element with the domain name to limit the action to only specified domains.
-- Use an Action element to allow or deny permission to call this action. You
-- cannot use an IAM policy to constrain this action's parameters. If the
-- caller does not have sufficient permissions to invoke the action, or the
-- parameter values fall outside the specified constraints, the action fails
-- by throwing OperationNotPermitted. For details and example IAM policies,
-- see Using IAM to Manage Access to Amazon SWF Workflows. ListWorkflowTypes
-- Example POST / HTTP/1.1 Host: swf.us-east-1.amazonaws.com User-Agent:
-- Mozilla/5.0 (Windows; U; Windows NT 6.1; en-US; rv:1.9.2.25) Gecko/20111212
-- Firefox/3.6.25 ( .NET CLR 3.5.30729; .NET4.0E) Accept: application/json,
-- text/javascript, */* Accept-Language: en-us,en;q=0.5 Accept-Encoding:
-- gzip,deflate Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.7 Keep-Alive: 115
-- Connection: keep-alive Content-Type: application/x-amz-json-1.0
-- X-Requested-With: XMLHttpRequest X-Amz-Date: Sun, 15 Jan 2012 22:25:43 GMT
-- X-Amz-Target: SimpleWorkflowService.ListWorkflowTypes Content-Encoding:
-- amz-1.0 X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,Algorithm=HmacSHA256,SignedHeaders=Host;X-Amz-Date;X-Amz-Target;Content-Encoding,Signature=uleWQSyVVf0+aG50IoBJG5h0hzxNFNT97Mkn/FSCQ+Q=
-- Referer: http://swf.us-east-1.amazonaws.com/explorer/index.html
-- Content-Length: 110 Pragma: no-cache Cache-Control: no-cache {"domain":
-- "867530901", "registrationStatus": "REGISTERED", "maximumPageSize": 50,
-- "reverseOrder": true} HTTP/1.1 200 OK Content-Length: 174 Content-Type:
-- application/json x-amzn-RequestId: dcde6719-3fc7-11e1-9e8f-57bb03e21482
-- {"typeInfos": [ {"creationDate": 1326481174.027, "description": "Handle
-- customer orders", "status": "REGISTERED", "workflowType": {"name":
-- "customerOrderWorkflow", "version": "1.0"} } ] }.
module Network.AWS.SWF.V2012_01_25.ListWorkflowTypes where

import Control.Lens
import Network.AWS.Request.JSON
import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListWorkflowTypes' request.
listWorkflowTypes :: Text -- ^ '_lwtiDomain'
                  -> RegistrationStatus -- ^ '_lwtiRegistrationStatus'
                  -> ListWorkflowTypes
listWorkflowTypes p1 p2 = ListWorkflowTypes
    { _lwtiDomain = p1
    , _lwtiRegistrationStatus = p2
    , _lwtiName = Nothing
    , _lwtiMaximumPageSize = Nothing
    , _lwtiNextPageToken = Nothing
    , _lwtiReverseOrder = Nothing
    }

data ListWorkflowTypes = ListWorkflowTypes
    { _lwtiDomain :: Text
      -- ^ The name of the domain in which the workflow types have been
      -- registered.
    , _lwtiRegistrationStatus :: RegistrationStatus
      -- ^ Specifies the registration status of the workflow types to list.
    , _lwtiName :: Maybe Text
      -- ^ If specified, lists the workflow type with this name.
    , _lwtiMaximumPageSize :: Maybe Integer
      -- ^ The maximum number of results returned in each page. The default
      -- is 100, but the caller can override this value to a page size
      -- smaller than the default. You cannot specify a page size greater
      -- than 100. Note that the number of types may be less than the
      -- maxiumum page size, in which case, the returned page will have
      -- fewer results than the maximumPageSize specified.
    , _lwtiNextPageToken :: Maybe Text
      -- ^ If on a previous call to this method a NextPageToken was
      -- returned, the results are being paginated. To get the next page
      -- of results, repeat the call with the returned token and all other
      -- arguments unchanged.
    , _lwtiReverseOrder :: Maybe Bool
      -- ^ When set to true, returns the results in reverse order. By
      -- default the results are returned in ascending alphabetical order
      -- of the name of the workflow types.
    } deriving (Generic)

makeLenses ''ListWorkflowTypes

instance ToPath ListWorkflowTypes

instance ToQuery ListWorkflowTypes

instance ToHeaders ListWorkflowTypes

instance ToJSON ListWorkflowTypes

data ListWorkflowTypesResponse = ListWorkflowTypesResponse
    { _wtiTypeInfos :: [WorkflowTypeInfo]
      -- ^ The list of workflow type information.
    , _wtiNextPageToken :: Maybe Text
      -- ^ The token for the next page of type information. If set then the
      -- list consists of more than one page. You can retrieve the next
      -- page by repeating the request (that returned the structure) with
      -- the this token and all other arguments unchanged.
    } deriving (Generic)

makeLenses ''ListWorkflowTypesResponse

instance FromJSON ListWorkflowTypesResponse

instance AWSRequest ListWorkflowTypes where
    type Sv ListWorkflowTypes = SWF
    type Rs ListWorkflowTypes = ListWorkflowTypesResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListWorkflowTypes where
    next rq rs = (\x -> rq { _lwtiNextPageToken = Just x })
        <$> (_wtiNextPageToken rs)
