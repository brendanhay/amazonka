{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SWF.ListActivityTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns information about all activities registered in the specified
-- domain that match the specified name and registration status. The result
-- includes information like creation date, current status of the activity,
-- etc. The results may be split into multiple pages. To retrieve
-- subsequent pages, make the call again using the @nextPageToken@ returned
-- by the initial call.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller does not have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s __cause__ parameter will
-- be set to OPERATION_NOT_PERMITTED. For details and example IAM policies,
-- see
-- <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>.
--
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_ListActivityTypes.html>
module Network.AWS.SWF.ListActivityTypes
    (
    -- * Request
      ListActivityTypes
    -- ** Request constructor
    , listActivityTypes
    -- ** Request lenses
    , latNextPageToken
    , latReverseOrder
    , latName
    , latMaximumPageSize
    , latDomain
    , latRegistrationStatus

    -- * Response
    , ListActivityTypesResponse
    -- ** Response constructor
    , listActivityTypesResponse
    -- ** Response lenses
    , latrNextPageToken
    , latrStatus
    , latrTypeInfos
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'listActivityTypes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'latNextPageToken'
--
-- * 'latReverseOrder'
--
-- * 'latName'
--
-- * 'latMaximumPageSize'
--
-- * 'latDomain'
--
-- * 'latRegistrationStatus'
data ListActivityTypes = ListActivityTypes'
    { _latNextPageToken      :: !(Maybe Text)
    , _latReverseOrder       :: !(Maybe Bool)
    , _latName               :: !(Maybe Text)
    , _latMaximumPageSize    :: !(Maybe Nat)
    , _latDomain             :: !Text
    , _latRegistrationStatus :: !RegistrationStatus
    } deriving (Eq,Read,Show)

-- | 'ListActivityTypes' smart constructor.
listActivityTypes :: Text -> RegistrationStatus -> ListActivityTypes
listActivityTypes pDomain pRegistrationStatus =
    ListActivityTypes'
    { _latNextPageToken = Nothing
    , _latReverseOrder = Nothing
    , _latName = Nothing
    , _latMaximumPageSize = Nothing
    , _latDomain = pDomain
    , _latRegistrationStatus = pRegistrationStatus
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
latNextPageToken :: Lens' ListActivityTypes (Maybe Text)
latNextPageToken = lens _latNextPageToken (\ s a -> s{_latNextPageToken = a});

-- | When set to @true@, returns the results in reverse order. By default,
-- the results are returned in ascending alphabetical order by @name@ of
-- the activity types.
latReverseOrder :: Lens' ListActivityTypes (Maybe Bool)
latReverseOrder = lens _latReverseOrder (\ s a -> s{_latReverseOrder = a});

-- | If specified, only lists the activity types that have this name.
latName :: Lens' ListActivityTypes (Maybe Text)
latName = lens _latName (\ s a -> s{_latName = a});

-- | The maximum number of results that will be returned per call.
-- @nextPageToken@ can be used to obtain futher pages of results. The
-- default is 100, which is the maximum allowed page size. You can,
-- however, specify a page size /smaller/ than 100.
--
-- This is an upper limit only; the actual number of results returned per
-- call may be fewer than the specified maximum.
latMaximumPageSize :: Lens' ListActivityTypes (Maybe Natural)
latMaximumPageSize = lens _latMaximumPageSize (\ s a -> s{_latMaximumPageSize = a}) . mapping _Nat;

-- | The name of the domain in which the activity types have been registered.
latDomain :: Lens' ListActivityTypes Text
latDomain = lens _latDomain (\ s a -> s{_latDomain = a});

-- | Specifies the registration status of the activity types to list.
latRegistrationStatus :: Lens' ListActivityTypes RegistrationStatus
latRegistrationStatus = lens _latRegistrationStatus (\ s a -> s{_latRegistrationStatus = a});

instance AWSPager ListActivityTypes where
        page rq rs
          | stop (rs ^. latrNextPageToken) = Nothing
          | stop (rs ^. latrTypeInfos) = Nothing
          | otherwise =
            Just $ rq &
              latNextPageToken .~ rs ^. latrNextPageToken

instance AWSRequest ListActivityTypes where
        type Sv ListActivityTypes = SWF
        type Rs ListActivityTypes = ListActivityTypesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListActivityTypesResponse' <$>
                   (x .?> "nextPageToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "typeInfos" .!@ mempty))

instance ToHeaders ListActivityTypes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.ListActivityTypes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListActivityTypes where
        toJSON ListActivityTypes'{..}
          = object
              ["nextPageToken" .= _latNextPageToken,
               "reverseOrder" .= _latReverseOrder,
               "name" .= _latName,
               "maximumPageSize" .= _latMaximumPageSize,
               "domain" .= _latDomain,
               "registrationStatus" .= _latRegistrationStatus]

instance ToPath ListActivityTypes where
        toPath = const "/"

instance ToQuery ListActivityTypes where
        toQuery = const mempty

-- | Contains a paginated list of activity type information structures.
--
-- /See:/ 'listActivityTypesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'latrNextPageToken'
--
-- * 'latrStatus'
--
-- * 'latrTypeInfos'
data ListActivityTypesResponse = ListActivityTypesResponse'
    { _latrNextPageToken :: !(Maybe Text)
    , _latrStatus        :: !Int
    , _latrTypeInfos     :: ![ActivityTypeInfo]
    } deriving (Eq,Read,Show)

-- | 'ListActivityTypesResponse' smart constructor.
listActivityTypesResponse :: Int -> ListActivityTypesResponse
listActivityTypesResponse pStatus =
    ListActivityTypesResponse'
    { _latrNextPageToken = Nothing
    , _latrStatus = pStatus
    , _latrTypeInfos = mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
latrNextPageToken :: Lens' ListActivityTypesResponse (Maybe Text)
latrNextPageToken = lens _latrNextPageToken (\ s a -> s{_latrNextPageToken = a});

-- | FIXME: Undocumented member.
latrStatus :: Lens' ListActivityTypesResponse Int
latrStatus = lens _latrStatus (\ s a -> s{_latrStatus = a});

-- | List of activity type information.
latrTypeInfos :: Lens' ListActivityTypesResponse [ActivityTypeInfo]
latrTypeInfos = lens _latrTypeInfos (\ s a -> s{_latrTypeInfos = a});
