{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.V2012_01_25.ListActivityTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about all activities registered in the specified domain
-- that match the specified name and registration status. The result includes
-- information like creation date, current status of the activity, etc. The
-- results may be split into multiple pages. To retrieve subsequent pages,
-- make the call again using the nextPageToken returned by the initial call.
-- Access Control You can use IAM policies to control this action's access to
-- Amazon SWF resources as follows: Use a Resource element with the domain
-- name to limit the action to only specified domains. Use an Action element
-- to allow or deny permission to call this action. You cannot use an IAM
-- policy to constrain this action's parameters. If the caller does not have
-- sufficient permissions to invoke the action, or the parameter values fall
-- outside the specified constraints, the action fails by throwing
-- OperationNotPermitted. For details and example IAM policies, see Using IAM
-- to Manage Access to Amazon SWF Workflows. ListActivityTypes Example
-- {"domain": "867530901", "registrationStatus": "REGISTERED",
-- "maximumPageSize": 50, "reverseOrder": false} HTTP/1.1 200 OK
-- Content-Length: 171 Content-Type: application/json x-amzn-RequestId:
-- 11b6fbeb-3f25-11e1-9e8f-57bb03e21482 {"typeInfos": [ {"activityType":
-- {"name": "activityVerify", "version": "1.0"}, "creationDate":
-- 1326586446.471, "description": "Verify the customer credit", "status":
-- "REGISTERED"} ] }.
module Network.AWS.SWF.V2012_01_25.ListActivityTypes where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.SWF.V2012_01_25.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

-- | Minimum specification for a 'ListActivityTypes' request.
listActivityTypes :: Text -- ^ '_latiDomain'
                  -> RegistrationStatus -- ^ '_latiRegistrationStatus'
                  -> ListActivityTypes
listActivityTypes p1 p2 = ListActivityTypes
    { _latiDomain = p1
    , _latiRegistrationStatus = p2
    , _latiName = Nothing
    , _latiMaximumPageSize = Nothing
    , _latiNextPageToken = Nothing
    , _latiReverseOrder = Nothing
    }

data ListActivityTypes = ListActivityTypes
    { _latiDomain :: Text
      -- ^ The name of the domain in which the activity types have been
      -- registered.
    , _latiRegistrationStatus :: RegistrationStatus
      -- ^ Specifies the registration status of the activity types to list.
    , _latiName :: Maybe Text
      -- ^ If specified, only lists the activity types that have this name.
    , _latiMaximumPageSize :: Maybe Integer
      -- ^ The maximum number of results returned in each page. The default
      -- is 100, but the caller can override this value to a page size
      -- smaller than the default. You cannot specify a page size greater
      -- than 100. Note that the number of types may be less than the
      -- maxiumum page size, in which case, the returned page will have
      -- fewer results than the maximumPageSize specified.
    , _latiNextPageToken :: Maybe Text
      -- ^ If on a previous call to this method a NextResultToken was
      -- returned, the results have more than one page. To get the next
      -- page of results, repeat the call with the nextPageToken and keep
      -- all other arguments unchanged.
    , _latiReverseOrder :: Maybe Bool
      -- ^ When set to true, returns the results in reverse order. By
      -- default the results are returned in ascending alphabetical order
      -- of the name of the activity types.
    } deriving (Show, Generic)

makeLenses ''ListActivityTypes

instance ToPath ListActivityTypes

instance ToQuery ListActivityTypes

instance ToHeaders ListActivityTypes

instance ToJSON ListActivityTypes

data ListActivityTypesResponse = ListActivityTypesResponse
    { _atiTypeInfos :: [ActivityTypeInfo]
      -- ^ List of activity type information.
    , _atiNextPageToken :: Maybe Text
      -- ^ Returns a value if the results are paginated. To get the next
      -- page of results, repeat the request specifying this token and all
      -- other arguments unchanged.
    } deriving (Show, Generic)

makeLenses ''ListActivityTypesResponse

instance FromJSON ListActivityTypesResponse

instance AWSRequest ListActivityTypes where
    type Sv ListActivityTypes = SWF
    type Rs ListActivityTypes = ListActivityTypesResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListActivityTypes where
    next rq rs = (\x -> rq { _latiNextPageToken = Just x })
        <$> (_atiNextPageToken rs)
