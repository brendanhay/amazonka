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
-- Module      : Network.AWS.SWF.ListDomains
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of domains registered in the account. The results may be split into multiple pages. To retrieve subsequent pages, make the call again using the nextPageToken returned by the initial call.
--
--
-- __Access Control__
--
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains. The element must be set to @arn:aws:swf::AccountID:domain/*@ , where /AccountID/ is the account ID, with no dashes.
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
module Network.AWS.SWF.ListDomains
    (
    -- * Creating a Request
      listDomains
    , ListDomains
    -- * Request Lenses
    , ldNextPageToken
    , ldReverseOrder
    , ldMaximumPageSize
    , ldRegistrationStatus

    -- * Destructuring the Response
    , listDomainsResponse
    , ListDomainsResponse
    -- * Response Lenses
    , ldrsNextPageToken
    , ldrsResponseStatus
    , ldrsDomainInfos
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SWF.Types
import Network.AWS.SWF.Types.Product

-- | /See:/ 'listDomains' smart constructor.
data ListDomains = ListDomains'
  { _ldNextPageToken      :: !(Maybe Text)
  , _ldReverseOrder       :: !(Maybe Bool)
  , _ldMaximumPageSize    :: !(Maybe Nat)
  , _ldRegistrationStatus :: !RegistrationStatus
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDomains' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldNextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- * 'ldReverseOrder' - When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the domains.
--
-- * 'ldMaximumPageSize' - The maximum number of results that are returned per call. @nextPageToken@ can be used to obtain futher pages of results. The default is 1000, which is the maximum allowed page size. You can, however, specify a page size /smaller/ than the maximum. This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
--
-- * 'ldRegistrationStatus' - Specifies the registration status of the domains to list.
listDomains
    :: RegistrationStatus -- ^ 'ldRegistrationStatus'
    -> ListDomains
listDomains pRegistrationStatus_ =
  ListDomains'
    { _ldNextPageToken = Nothing
    , _ldReverseOrder = Nothing
    , _ldMaximumPageSize = Nothing
    , _ldRegistrationStatus = pRegistrationStatus_
    }


-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
ldNextPageToken :: Lens' ListDomains (Maybe Text)
ldNextPageToken = lens _ldNextPageToken (\ s a -> s{_ldNextPageToken = a})

-- | When set to @true@ , returns the results in reverse order. By default, the results are returned in ascending alphabetical order by @name@ of the domains.
ldReverseOrder :: Lens' ListDomains (Maybe Bool)
ldReverseOrder = lens _ldReverseOrder (\ s a -> s{_ldReverseOrder = a})

-- | The maximum number of results that are returned per call. @nextPageToken@ can be used to obtain futher pages of results. The default is 1000, which is the maximum allowed page size. You can, however, specify a page size /smaller/ than the maximum. This is an upper limit only; the actual number of results returned per call may be fewer than the specified maximum.
ldMaximumPageSize :: Lens' ListDomains (Maybe Natural)
ldMaximumPageSize = lens _ldMaximumPageSize (\ s a -> s{_ldMaximumPageSize = a}) . mapping _Nat

-- | Specifies the registration status of the domains to list.
ldRegistrationStatus :: Lens' ListDomains RegistrationStatus
ldRegistrationStatus = lens _ldRegistrationStatus (\ s a -> s{_ldRegistrationStatus = a})

instance AWSPager ListDomains where
        page rq rs
          | stop (rs ^. ldrsNextPageToken) = Nothing
          | stop (rs ^. ldrsDomainInfos) = Nothing
          | otherwise =
            Just $ rq &
              ldNextPageToken .~ rs ^. ldrsNextPageToken

instance AWSRequest ListDomains where
        type Rs ListDomains = ListDomainsResponse
        request = postJSON swf
        response
          = receiveJSON
              (\ s h x ->
                 ListDomainsResponse' <$>
                   (x .?> "nextPageToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "domainInfos" .!@ mempty))

instance Hashable ListDomains where

instance NFData ListDomains where

instance ToHeaders ListDomains where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SimpleWorkflowService.ListDomains" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.0" :: ByteString)])

instance ToJSON ListDomains where
        toJSON ListDomains'{..}
          = object
              (catMaybes
                 [("nextPageToken" .=) <$> _ldNextPageToken,
                  ("reverseOrder" .=) <$> _ldReverseOrder,
                  ("maximumPageSize" .=) <$> _ldMaximumPageSize,
                  Just
                    ("registrationStatus" .= _ldRegistrationStatus)])

instance ToPath ListDomains where
        toPath = const "/"

instance ToQuery ListDomains where
        toQuery = const mempty

-- | Contains a paginated collection of DomainInfo structures.
--
--
--
-- /See:/ 'listDomainsResponse' smart constructor.
data ListDomainsResponse = ListDomainsResponse'
  { _ldrsNextPageToken  :: !(Maybe Text)
  , _ldrsResponseStatus :: !Int
  , _ldrsDomainInfos    :: ![DomainInfo]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListDomainsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldrsNextPageToken' - If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- * 'ldrsResponseStatus' - -- | The response status code.
--
-- * 'ldrsDomainInfos' - A list of DomainInfo structures.
listDomainsResponse
    :: Int -- ^ 'ldrsResponseStatus'
    -> ListDomainsResponse
listDomainsResponse pResponseStatus_ =
  ListDomainsResponse'
    { _ldrsNextPageToken = Nothing
    , _ldrsResponseStatus = pResponseStatus_
    , _ldrsDomainInfos = mempty
    }


-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged. The configured @maximumPageSize@ determines how many results can be returned in a single call.
ldrsNextPageToken :: Lens' ListDomainsResponse (Maybe Text)
ldrsNextPageToken = lens _ldrsNextPageToken (\ s a -> s{_ldrsNextPageToken = a})

-- | -- | The response status code.
ldrsResponseStatus :: Lens' ListDomainsResponse Int
ldrsResponseStatus = lens _ldrsResponseStatus (\ s a -> s{_ldrsResponseStatus = a})

-- | A list of DomainInfo structures.
ldrsDomainInfos :: Lens' ListDomainsResponse [DomainInfo]
ldrsDomainInfos = lens _ldrsDomainInfos (\ s a -> s{_ldrsDomainInfos = a}) . _Coerce

instance NFData ListDomainsResponse where
