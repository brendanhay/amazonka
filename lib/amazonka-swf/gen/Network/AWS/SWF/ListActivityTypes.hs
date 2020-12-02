{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListActivityTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all activities registered in the specified domain that match the specified name and registration status. The result includes information like creation date, current status of the activity, etc. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the @nextPageToken@ returned by the initial call.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <http://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
--
--
-- This operation returns paginated results.
module Network.AWS.SWF.ListActivityTypes
    (
    -- * Creating a Request
      listActivityTypes
    , ListActivityTypes
    -- * Request Lenses
    , latNextPageToken
    , latReverseOrder
    , latName
    , latMaximumPageSize
    , latDomain
    , latRegistrationStatus

    -- * Destructuring the Response
    , listActivityTypesResponse
    , ListActivityTypesResponse
    -- * Response Lenses
    , latrsNextPageToken
    , latrsResponseStatus
    , latrsTypeInfos
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'listActivityTypes' smart constructor.
data ListActivityTypes = ListActivityTypes'
  { _latNextPageToken      :: !(Maybe Text)
  , _latReverseOrder       :: !(Maybe Bool)
  , _latName               :: !(Maybe Text)
  , _latMaximumPageSize    :: !(Maybe Nat)
  , _latDomain             :: !Text
  , _latRegistrationStatus :: !RegistrationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListActivityTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'latNextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- * 'latReverseOrder' - When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the activity types.
--
-- * 'latName' - If specified, only lists the activity types that have this name.
--
-- * 'latMaximumPageSize' - The maximum number of results that are returned per call. @nextPageToken@ can be used to obtain futher pages of results. The default is 1000, which is the maximum allowed page size. You can, however, specify a page size /smaller/ than the maximum. This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
--
-- * 'latDomain' - The name of the domain in which the activity types have been registered.
--
-- * 'latRegistrationStatus' - Specifies the registration status of the activity types to list.
listActivityTypes
    :: Text -- ^ 'latDomain'
    -> RegistrationStatus -- ^ 'latRegistrationStatus'
    -> ListActivityTypes
listActivityTypes pDomain_ pRegistrationStatus_ =
  ListActivityTypes'
    { _latNextPageToken = Nothing
    , _latReverseOrder = Nothing
    , _latName = Nothing
    , _latMaximumPageSize = Nothing
    , _latDomain = pDomain_
    , _latRegistrationStatus = pRegistrationStatus_
    }


-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
latNextPageToken :: Lens' ListActivityTypes (Maybe Text)
latNextPageToken = lens _latNextPageToken (\ s a -> s{_latNextPageToken = a})

-- | When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the activity types.
latReverseOrder :: Lens' ListActivityTypes (Maybe Bool)
latReverseOrder = lens _latReverseOrder (\ s a -> s{_latReverseOrder = a})

-- | If specified, only lists the activity types that have this name.
latName :: Lens' ListActivityTypes (Maybe Text)
latName = lens _latName (\ s a -> s{_latName = a})

-- | The maximum number of results that are returned per call. @nextPageToken@ can be used to obtain futher pages of results. The default is 1000, which is the maximum allowed page size. You can, however, specify a page size /smaller/ than the maximum. This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
latMaximumPageSize :: Lens' ListActivityTypes (Maybe Natural)
latMaximumPageSize = lens _latMaximumPageSize (\ s a -> s{_latMaximumPageSize = a}) . mapping _Nat

-- | The name of the domain in which the activity types have been registered.
latDomain :: Lens' ListActivityTypes Text
latDomain = lens _latDomain (\ s a -> s{_latDomain = a})

-- | Specifies the registration status of the activity types to list.
latRegistrationStatus :: Lens' ListActivityTypes RegistrationStatus
latRegistrationStatus = lens _latRegistrationStatus (\ s a -> s{_latRegistrationStatus = a})

instance AWSPager ListActivityTypes where
        page rq rs
          | stop (rs ^. latrsNextPageToken) = Nothing
          | stop (rs ^. latrsTypeInfos) = Nothing
          | otherwise =
            Just $ rq &
              latNextPageToken .~ rs ^. latrsNextPageToken

instance AWSRequest ListActivityTypes where
        type Rs ListActivityTypes = ListActivityTypesResponse
        request = postJSON swf
        response
          = receiveJSON
              (\ s h x ->
                 ListActivityTypesResponse' <$>
                   (x .?> "nextPageToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "typeInfos" .!@ mempty))

instance Hashable ListActivityTypes where

instance NFData ListActivityTypes where

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
              (catMaybes
                 [("nextPageToken" .=) <$> _latNextPageToken,
                  ("reverseOrder" .=) <$> _latReverseOrder,
                  ("name" .=) <$> _latName,
                  ("maximumPageSize" .=) <$> _latMaximumPageSize,
                  Just ("domain" .= _latDomain),
                  Just
                    ("registrationStatus" .= _latRegistrationStatus)])

instance ToPath ListActivityTypes where
        toPath = const "/"

instance ToQuery ListActivityTypes where
        toQuery = const mempty

-- | Contains a paginated list of activity type information structures.
--
--
--
-- /See:/ 'listActivityTypesResponse' smart constructor.
data ListActivityTypesResponse = ListActivityTypesResponse'
  { _latrsNextPageToken  :: !(Maybe Text)
  , _latrsResponseStatus :: !Int
  , _latrsTypeInfos      :: ![ActivityTypeInfo]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListActivityTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'latrsNextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- * 'latrsResponseStatus' - -- | The response status code.
--
-- * 'latrsTypeInfos' - List of activity type information.
listActivityTypesResponse
    :: Int -- ^ 'latrsResponseStatus'
    -> ListActivityTypesResponse
listActivityTypesResponse pResponseStatus_ =
  ListActivityTypesResponse'
    { _latrsNextPageToken = Nothing
    , _latrsResponseStatus = pResponseStatus_
    , _latrsTypeInfos = mempty
    }


-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
latrsNextPageToken :: Lens' ListActivityTypesResponse (Maybe Text)
latrsNextPageToken = lens _latrsNextPageToken (\ s a -> s{_latrsNextPageToken = a})

-- | -- | The response status code.
latrsResponseStatus :: Lens' ListActivityTypesResponse Int
latrsResponseStatus = lens _latrsResponseStatus (\ s a -> s{_latrsResponseStatus = a})

-- | List of activity type information.
latrsTypeInfos :: Lens' ListActivityTypesResponse [ActivityTypeInfo]
latrsTypeInfos = lens _latrsTypeInfos (\ s a -> s{_latrsTypeInfos = a}) . _Coerce

instance NFData ListActivityTypesResponse where
