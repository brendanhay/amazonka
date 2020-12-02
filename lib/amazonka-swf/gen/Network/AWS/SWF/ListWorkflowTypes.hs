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
-- Module      : Network.AWS.SWF.ListWorkflowTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about workflow types in the specified domain. The results may be split into multiple pages that can be retrieved by making the call repeatedly.
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
module Network.AWS.SWF.ListWorkflowTypes
    (
    -- * Creating a Request
      listWorkflowTypes
    , ListWorkflowTypes
    -- * Request Lenses
    , lwtNextPageToken
    , lwtReverseOrder
    , lwtName
    , lwtMaximumPageSize
    , lwtDomain
    , lwtRegistrationStatus

    -- * Destructuring the Response
    , listWorkflowTypesResponse
    , ListWorkflowTypesResponse
    -- * Response Lenses
    , lwtrsNextPageToken
    , lwtrsResponseStatus
    , lwtrsTypeInfos
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'listWorkflowTypes' smart constructor.
data ListWorkflowTypes = ListWorkflowTypes'
  { _lwtNextPageToken      :: !(Maybe Text)
  , _lwtReverseOrder       :: !(Maybe Bool)
  , _lwtName               :: !(Maybe Text)
  , _lwtMaximumPageSize    :: !(Maybe Nat)
  , _lwtDomain             :: !Text
  , _lwtRegistrationStatus :: !RegistrationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListWorkflowTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwtNextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- * 'lwtReverseOrder' - When set to @true@ , returns the results in reverse order. By default the results are returned in ascending alphabetical order of the @name@ of the workflow types.
--
-- * 'lwtName' - If specified, lists the workflow type with this name.
--
-- * 'lwtMaximumPageSize' - The maximum number of results that are returned per call. @nextPageToken@ can be used to obtain futher pages of results. The default is 1000, which is the maximum allowed page size. You can, however, specify a page size /smaller/ than the maximum. This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
--
-- * 'lwtDomain' - The name of the domain in which the workflow types have been registered.
--
-- * 'lwtRegistrationStatus' - Specifies the registration status of the workflow types to list.
listWorkflowTypes
    :: Text -- ^ 'lwtDomain'
    -> RegistrationStatus -- ^ 'lwtRegistrationStatus'
    -> ListWorkflowTypes
listWorkflowTypes pDomain_ pRegistrationStatus_ =
  ListWorkflowTypes'
    { _lwtNextPageToken = Nothing
    , _lwtReverseOrder = Nothing
    , _lwtName = Nothing
    , _lwtMaximumPageSize = Nothing
    , _lwtDomain = pDomain_
    , _lwtRegistrationStatus = pRegistrationStatus_
    }


-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
lwtNextPageToken :: Lens' ListWorkflowTypes (Maybe Text)
lwtNextPageToken = lens _lwtNextPageToken (\ s a -> s{_lwtNextPageToken = a})

-- | When set to @true@ , returns the results in reverse order. By default the results are returned in ascending alphabetical order of the @name@ of the workflow types.
lwtReverseOrder :: Lens' ListWorkflowTypes (Maybe Bool)
lwtReverseOrder = lens _lwtReverseOrder (\ s a -> s{_lwtReverseOrder = a})

-- | If specified, lists the workflow type with this name.
lwtName :: Lens' ListWorkflowTypes (Maybe Text)
lwtName = lens _lwtName (\ s a -> s{_lwtName = a})

-- | The maximum number of results that are returned per call. @nextPageToken@ can be used to obtain futher pages of results. The default is 1000, which is the maximum allowed page size. You can, however, specify a page size /smaller/ than the maximum. This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
lwtMaximumPageSize :: Lens' ListWorkflowTypes (Maybe Natural)
lwtMaximumPageSize = lens _lwtMaximumPageSize (\ s a -> s{_lwtMaximumPageSize = a}) . mapping _Nat

-- | The name of the domain in which the workflow types have been registered.
lwtDomain :: Lens' ListWorkflowTypes Text
lwtDomain = lens _lwtDomain (\ s a -> s{_lwtDomain = a})

-- | Specifies the registration status of the workflow types to list.
lwtRegistrationStatus :: Lens' ListWorkflowTypes RegistrationStatus
lwtRegistrationStatus = lens _lwtRegistrationStatus (\ s a -> s{_lwtRegistrationStatus = a})

instance AWSPager ListWorkflowTypes where
        page rq rs
          | stop (rs ^. lwtrsNextPageToken) = Nothing
          | stop (rs ^. lwtrsTypeInfos) = Nothing
          | otherwise =
            Just $ rq &
              lwtNextPageToken .~ rs ^. lwtrsNextPageToken

instance AWSRequest ListWorkflowTypes where
        type Rs ListWorkflowTypes = ListWorkflowTypesResponse
        request = postJSON swf
        response
          = receiveJSON
              (\ s h x ->
                 ListWorkflowTypesResponse' <$>
                   (x .?> "nextPageToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "typeInfos" .!@ mempty))

instance Hashable ListWorkflowTypes where

instance NFData ListWorkflowTypes where

instance ToHeaders ListWorkflowTypes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.ListWorkflowTypes" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListWorkflowTypes where
        toJSON ListWorkflowTypes'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _lwtNextPageToken,
                  ("reverseOrder" .=) <$> _lwtReverseOrder,
                  ("name" .=) <$> _lwtName,
                  ("maximumPageSize" .=) <$> _lwtMaximumPageSize,
                  Just ("domain" .= _lwtDomain),
                  Just
                    ("registrationStatus" .= _lwtRegistrationStatus)])

instance ToPath ListWorkflowTypes where
        toPath = const "/"

instance ToQuery ListWorkflowTypes where
        toQuery = const mempty

-- | Contains a paginated list of information structures about workflow types.
--
--
--
-- /See:/ 'listWorkflowTypesResponse' smart constructor.
data ListWorkflowTypesResponse = ListWorkflowTypesResponse'
  { _lwtrsNextPageToken  :: !(Maybe Text)
  , _lwtrsResponseStatus :: !Int
  , _lwtrsTypeInfos      :: ![WorkflowTypeInfo]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListWorkflowTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwtrsNextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- * 'lwtrsResponseStatus' - -- | The response status code.
--
-- * 'lwtrsTypeInfos' - The list of workflow type information.
listWorkflowTypesResponse
    :: Int -- ^ 'lwtrsResponseStatus'
    -> ListWorkflowTypesResponse
listWorkflowTypesResponse pResponseStatus_ =
  ListWorkflowTypesResponse'
    { _lwtrsNextPageToken = Nothing
    , _lwtrsResponseStatus = pResponseStatus_
    , _lwtrsTypeInfos = mempty
    }


-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
lwtrsNextPageToken :: Lens' ListWorkflowTypesResponse (Maybe Text)
lwtrsNextPageToken = lens _lwtrsNextPageToken (\ s a -> s{_lwtrsNextPageToken = a})

-- | -- | The response status code.
lwtrsResponseStatus :: Lens' ListWorkflowTypesResponse Int
lwtrsResponseStatus = lens _lwtrsResponseStatus (\ s a -> s{_lwtrsResponseStatus = a})

-- | The list of workflow type information.
lwtrsTypeInfos :: Lens' ListWorkflowTypesResponse [WorkflowTypeInfo]
lwtrsTypeInfos = lens _lwtrsTypeInfos (\ s a -> s{_lwtrsTypeInfos = a}) . _Coerce

instance NFData ListWorkflowTypesResponse where
