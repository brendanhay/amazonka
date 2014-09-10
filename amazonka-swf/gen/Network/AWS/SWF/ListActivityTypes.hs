{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SWF.ListActivityTypes
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
module Network.AWS.SWF
    (
    -- * Request
      ListActivityTypes
    -- ** Request constructor
    , mkListActivityTypes
    -- ** Request lenses
    , latDomain
    , latName
    , latRegistrationStatus
    , latNextPageToken
    , latMaximumPageSize
    , latReverseOrder

    -- * Response
    , ListActivityTypesResponse
    -- ** Response constructor
    , mkListActivityTypesResponse
    -- ** Response lenses
    , latrTypeInfos
    , latrNextPageToken
    ) where

import Network.AWS.SWF.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data ListActivityTypes = ListActivityTypes
    { _latDomain :: !Text
    , _latName :: !(Maybe Text)
    , _latRegistrationStatus :: RegistrationStatus
    , _latNextPageToken :: !(Maybe Text)
    , _latMaximumPageSize :: !(Maybe Integer)
    , _latReverseOrder :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListActivityTypes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Domain ::@ @Text@
--
-- * @Name ::@ @Maybe Text@
--
-- * @RegistrationStatus ::@ @RegistrationStatus@
--
-- * @NextPageToken ::@ @Maybe Text@
--
-- * @MaximumPageSize ::@ @Maybe Integer@
--
-- * @ReverseOrder ::@ @Maybe Bool@
--
mkListActivityTypes :: Text -- ^ 'latDomain'
                    -> RegistrationStatus -- ^ 'latRegistrationStatus'
                    -> ListActivityTypes
mkListActivityTypes p1 p3 = ListActivityTypes
    { _latDomain = p1
    , _latName = Nothing
    , _latRegistrationStatus = p3
    , _latNextPageToken = Nothing
    , _latMaximumPageSize = Nothing
    , _latReverseOrder = Nothing
    }

-- | The name of the domain in which the activity types have been registered.
latDomain :: Lens' ListActivityTypes Text
latDomain = lens _latDomain (\s a -> s { _latDomain = a })

-- | If specified, only lists the activity types that have this name.
latName :: Lens' ListActivityTypes (Maybe Text)
latName = lens _latName (\s a -> s { _latName = a })

-- | Specifies the registration status of the activity types to list.
latRegistrationStatus :: Lens' ListActivityTypes RegistrationStatus
latRegistrationStatus =
    lens _latRegistrationStatus (\s a -> s { _latRegistrationStatus = a })

-- | If on a previous call to this method a NextResultToken was returned, the
-- results have more than one page. To get the next page of results, repeat
-- the call with the nextPageToken and keep all other arguments unchanged.
latNextPageToken :: Lens' ListActivityTypes (Maybe Text)
latNextPageToken =
    lens _latNextPageToken (\s a -> s { _latNextPageToken = a })

-- | The maximum number of results returned in each page. The default is 100,
-- but the caller can override this value to a page size smaller than the
-- default. You cannot specify a page size greater than 100. Note that the
-- number of types may be less than the maxiumum page size, in which case, the
-- returned page will have fewer results than the maximumPageSize specified.
latMaximumPageSize :: Lens' ListActivityTypes (Maybe Integer)
latMaximumPageSize =
    lens _latMaximumPageSize (\s a -> s { _latMaximumPageSize = a })

-- | When set to true, returns the results in reverse order. By default the
-- results are returned in ascending alphabetical order of the name of the
-- activity types.
latReverseOrder :: Lens' ListActivityTypes (Maybe Bool)
latReverseOrder = lens _latReverseOrder (\s a -> s { _latReverseOrder = a })

instance ToPath ListActivityTypes

instance ToQuery ListActivityTypes

instance ToHeaders ListActivityTypes

instance ToJSON ListActivityTypes

-- | Contains a paginated list of activity type information structures.
data ListActivityTypesResponse = ListActivityTypesResponse
    { _latrTypeInfos :: [ActivityTypeInfo]
    , _latrNextPageToken :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListActivityTypesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TypeInfos ::@ @[ActivityTypeInfo]@
--
-- * @NextPageToken ::@ @Maybe Text@
--
mkListActivityTypesResponse :: [ActivityTypeInfo] -- ^ 'latrTypeInfos'
                            -> ListActivityTypesResponse
mkListActivityTypesResponse p1 = ListActivityTypesResponse
    { _latrTypeInfos = p1
    , _latrNextPageToken = Nothing
    }

-- | List of activity type information.
latrTypeInfos :: Lens' ListActivityTypesResponse [ActivityTypeInfo]
latrTypeInfos = lens _latrTypeInfos (\s a -> s { _latrTypeInfos = a })

-- | Returns a value if the results are paginated. To get the next page of
-- results, repeat the request specifying this token and all other arguments
-- unchanged.
latrNextPageToken :: Lens' ListActivityTypesResponse (Maybe Text)
latrNextPageToken =
    lens _latrNextPageToken (\s a -> s { _latrNextPageToken = a })

instance FromJSON ListActivityTypesResponse

instance AWSRequest ListActivityTypes where
    type Sv ListActivityTypes = SWF
    type Rs ListActivityTypes = ListActivityTypesResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListActivityTypes where
    next rq rs = (\x -> rq & latNextPageToken ?~ x)
        <$> (rs ^. latrNextPageToken)
