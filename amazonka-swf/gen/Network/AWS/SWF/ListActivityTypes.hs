{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
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
-- to Manage Access to Amazon SWF Workflows.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_ListActivityTypes.html>
module Network.AWS.SWF.ListActivityTypes
    (
    -- * Request
      ListActivityTypes
    -- ** Request constructor
    , listActivityTypes
    -- ** Request lenses
    , latDomain
    , latMaximumPageSize
    , latName
    , latNextPageToken
    , latRegistrationStatus
    , latReverseOrder

    -- * Response
    , ListActivityTypesResponse
    -- ** Response constructor
    , listActivityTypesResponse
    -- ** Response lenses
    , latrNextPageToken
    , latrTypeInfos
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.SWF.Types
import qualified GHC.Exts

data ListActivityTypes = ListActivityTypes
    { _latDomain             :: Text
    , _latMaximumPageSize    :: Maybe Nat
    , _latName               :: Maybe Text
    , _latNextPageToken      :: Maybe Text
    , _latRegistrationStatus :: Text
    , _latReverseOrder       :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListActivityTypes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'latDomain' @::@ 'Text'
--
-- * 'latMaximumPageSize' @::@ 'Maybe' 'Natural'
--
-- * 'latName' @::@ 'Maybe' 'Text'
--
-- * 'latNextPageToken' @::@ 'Maybe' 'Text'
--
-- * 'latRegistrationStatus' @::@ 'Text'
--
-- * 'latReverseOrder' @::@ 'Maybe' 'Bool'
--
listActivityTypes :: Text -- ^ 'latDomain'
                  -> Text -- ^ 'latRegistrationStatus'
                  -> ListActivityTypes
listActivityTypes p1 p2 = ListActivityTypes
    { _latDomain             = p1
    , _latRegistrationStatus = p2
    , _latName               = Nothing
    , _latNextPageToken      = Nothing
    , _latMaximumPageSize    = Nothing
    , _latReverseOrder       = Nothing
    }

-- | The name of the domain in which the activity types have been registered.
latDomain :: Lens' ListActivityTypes Text
latDomain = lens _latDomain (\s a -> s { _latDomain = a })

-- | The maximum number of results returned in each page. The default is 100,
-- but the caller can override this value to a page size smaller than the
-- default. You cannot specify a page size greater than 100. Note that the
-- number of types may be less than the maxiumum page size, in which case,
-- the returned page will have fewer results than the maximumPageSize
-- specified.
latMaximumPageSize :: Lens' ListActivityTypes (Maybe Natural)
latMaximumPageSize =
    lens _latMaximumPageSize (\s a -> s { _latMaximumPageSize = a })
        . mapping _Nat

-- | If specified, only lists the activity types that have this name.
latName :: Lens' ListActivityTypes (Maybe Text)
latName = lens _latName (\s a -> s { _latName = a })

-- | If on a previous call to this method a NextResultToken was returned, the
-- results have more than one page. To get the next page of results, repeat
-- the call with the nextPageToken and keep all other arguments unchanged.
latNextPageToken :: Lens' ListActivityTypes (Maybe Text)
latNextPageToken = lens _latNextPageToken (\s a -> s { _latNextPageToken = a })

-- | Specifies the registration status of the activity types to list.
latRegistrationStatus :: Lens' ListActivityTypes Text
latRegistrationStatus =
    lens _latRegistrationStatus (\s a -> s { _latRegistrationStatus = a })

-- | When set to true, returns the results in reverse order. By default the
-- results are returned in ascending alphabetical order of the name of the
-- activity types.
latReverseOrder :: Lens' ListActivityTypes (Maybe Bool)
latReverseOrder = lens _latReverseOrder (\s a -> s { _latReverseOrder = a })

data ListActivityTypesResponse = ListActivityTypesResponse
    { _latrNextPageToken :: Maybe Text
    , _latrTypeInfos     :: [ActivityTypeInfo]
    } deriving (Eq, Show, Generic)

-- | 'ListActivityTypesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'latrNextPageToken' @::@ 'Maybe' 'Text'
--
-- * 'latrTypeInfos' @::@ ['ActivityTypeInfo']
--
listActivityTypesResponse :: ListActivityTypesResponse
listActivityTypesResponse = ListActivityTypesResponse
    { _latrTypeInfos     = mempty
    , _latrNextPageToken = Nothing
    }

-- | Returns a value if the results are paginated. To get the next page of
-- results, repeat the request specifying this token and all other arguments
-- unchanged.
latrNextPageToken :: Lens' ListActivityTypesResponse (Maybe Text)
latrNextPageToken =
    lens _latrNextPageToken (\s a -> s { _latrNextPageToken = a })

-- | List of activity type information.
latrTypeInfos :: Lens' ListActivityTypesResponse [ActivityTypeInfo]
latrTypeInfos = lens _latrTypeInfos (\s a -> s { _latrTypeInfos = a })

instance ToPath ListActivityTypes where
    toPath = const "/"

instance ToQuery ListActivityTypes where
    toQuery = const mempty

instance ToHeaders ListActivityTypes

instance ToJSON ListActivityTypes where
    toJSON ListActivityTypes{..} = object
        [ "domain"             .= _latDomain
        , "name"               .= _latName
        , "registrationStatus" .= _latRegistrationStatus
        , "nextPageToken"      .= _latNextPageToken
        , "maximumPageSize"    .= _latMaximumPageSize
        , "reverseOrder"       .= _latReverseOrder
        ]

instance AWSRequest ListActivityTypes where
    type Sv ListActivityTypes = SWF
    type Rs ListActivityTypes = ListActivityTypesResponse

    request  = post "ListActivityTypes"
    response = jsonResponse

instance FromJSON ListActivityTypesResponse where
    parseJSON = withObject "ListActivityTypesResponse" $ \o -> ListActivityTypesResponse
        <$> o .: "nextPageToken"
        <*> o .: "typeInfos"
