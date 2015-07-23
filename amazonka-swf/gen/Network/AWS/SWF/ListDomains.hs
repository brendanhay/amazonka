{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListDomains
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of domains registered in the account. The results may
-- be split into multiple pages. To retrieve subsequent pages, make the
-- call again using the nextPageToken returned by the initial call.
--
-- This operation is eventually consistent. The results are best effort and
-- may not exactly reflect recent updates and changes.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains. The element must be set to
--     @arn:aws:swf::AccountID:domain\/*@, where /AccountID/ is the account
--     ID, with no dashes.
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
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_ListDomains.html>
module Network.AWS.SWF.ListDomains
    (
    -- * Request
      ListDomains
    -- ** Request constructor
    , listDomains
    -- ** Request lenses
    , ldrqNextPageToken
    , ldrqReverseOrder
    , ldrqMaximumPageSize
    , ldrqRegistrationStatus

    -- * Response
    , ListDomainsResponse
    -- ** Response constructor
    , listDomainsResponse
    -- ** Response lenses
    , ldrsNextPageToken
    , ldrsStatus
    , ldrsDomainInfos
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'listDomains' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrqNextPageToken'
--
-- * 'ldrqReverseOrder'
--
-- * 'ldrqMaximumPageSize'
--
-- * 'ldrqRegistrationStatus'
data ListDomains = ListDomains'
    { _ldrqNextPageToken      :: !(Maybe Text)
    , _ldrqReverseOrder       :: !(Maybe Bool)
    , _ldrqMaximumPageSize    :: !(Maybe Nat)
    , _ldrqRegistrationStatus :: !RegistrationStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDomains' smart constructor.
listDomains :: RegistrationStatus -> ListDomains
listDomains pRegistrationStatus_ =
    ListDomains'
    { _ldrqNextPageToken = Nothing
    , _ldrqReverseOrder = Nothing
    , _ldrqMaximumPageSize = Nothing
    , _ldrqRegistrationStatus = pRegistrationStatus_
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
ldrqNextPageToken :: Lens' ListDomains (Maybe Text)
ldrqNextPageToken = lens _ldrqNextPageToken (\ s a -> s{_ldrqNextPageToken = a});

-- | When set to @true@, returns the results in reverse order. By default,
-- the results are returned in ascending alphabetical order by @name@ of
-- the domains.
ldrqReverseOrder :: Lens' ListDomains (Maybe Bool)
ldrqReverseOrder = lens _ldrqReverseOrder (\ s a -> s{_ldrqReverseOrder = a});

-- | The maximum number of results that will be returned per call.
-- @nextPageToken@ can be used to obtain futher pages of results. The
-- default is 100, which is the maximum allowed page size. You can,
-- however, specify a page size /smaller/ than 100.
--
-- This is an upper limit only; the actual number of results returned per
-- call may be fewer than the specified maximum.
ldrqMaximumPageSize :: Lens' ListDomains (Maybe Natural)
ldrqMaximumPageSize = lens _ldrqMaximumPageSize (\ s a -> s{_ldrqMaximumPageSize = a}) . mapping _Nat;

-- | Specifies the registration status of the domains to list.
ldrqRegistrationStatus :: Lens' ListDomains RegistrationStatus
ldrqRegistrationStatus = lens _ldrqRegistrationStatus (\ s a -> s{_ldrqRegistrationStatus = a});

instance AWSPager ListDomains where
        page rq rs
          | stop (rs ^. ldrsNextPageToken) = Nothing
          | stop (rs ^. ldrsDomainInfos) = Nothing
          | otherwise =
            Just $ rq &
              ldrqNextPageToken .~ rs ^. ldrsNextPageToken

instance AWSRequest ListDomains where
        type Sv ListDomains = SWF
        type Rs ListDomains = ListDomainsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListDomainsResponse' <$>
                   (x .?> "nextPageToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "domainInfos" .!@ mempty))

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
              ["nextPageToken" .= _ldrqNextPageToken,
               "reverseOrder" .= _ldrqReverseOrder,
               "maximumPageSize" .= _ldrqMaximumPageSize,
               "registrationStatus" .= _ldrqRegistrationStatus]

instance ToPath ListDomains where
        toPath = const "/"

instance ToQuery ListDomains where
        toQuery = const mempty

-- | Contains a paginated collection of DomainInfo structures.
--
-- /See:/ 'listDomainsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ldrsNextPageToken'
--
-- * 'ldrsStatus'
--
-- * 'ldrsDomainInfos'
data ListDomainsResponse = ListDomainsResponse'
    { _ldrsNextPageToken :: !(Maybe Text)
    , _ldrsStatus        :: !Int
    , _ldrsDomainInfos   :: ![DomainInfo]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListDomainsResponse' smart constructor.
listDomainsResponse :: Int -> ListDomainsResponse
listDomainsResponse pStatus_ =
    ListDomainsResponse'
    { _ldrsNextPageToken = Nothing
    , _ldrsStatus = pStatus_
    , _ldrsDomainInfos = mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
ldrsNextPageToken :: Lens' ListDomainsResponse (Maybe Text)
ldrsNextPageToken = lens _ldrsNextPageToken (\ s a -> s{_ldrsNextPageToken = a});

-- | FIXME: Undocumented member.
ldrsStatus :: Lens' ListDomainsResponse Int
ldrsStatus = lens _ldrsStatus (\ s a -> s{_ldrsStatus = a});

-- | A list of DomainInfo structures.
ldrsDomainInfos :: Lens' ListDomainsResponse [DomainInfo]
ldrsDomainInfos = lens _ldrsDomainInfos (\ s a -> s{_ldrsDomainInfos = a});
