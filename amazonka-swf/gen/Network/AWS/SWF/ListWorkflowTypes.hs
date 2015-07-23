{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.ListWorkflowTypes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about workflow types in the specified domain. The
-- results may be split into multiple pages that can be retrieved by making
-- the call repeatedly.
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
-- <http://docs.aws.amazon.com/amazonswf/latest/apireference/API_ListWorkflowTypes.html>
module Network.AWS.SWF.ListWorkflowTypes
    (
    -- * Request
      ListWorkflowTypes
    -- ** Request constructor
    , listWorkflowTypes
    -- ** Request lenses
    , lwtrqNextPageToken
    , lwtrqReverseOrder
    , lwtrqName
    , lwtrqMaximumPageSize
    , lwtrqDomain
    , lwtrqRegistrationStatus

    -- * Response
    , ListWorkflowTypesResponse
    -- ** Response constructor
    , listWorkflowTypesResponse
    -- ** Response lenses
    , lwtrsNextPageToken
    , lwtrsStatus
    , lwtrsTypeInfos
    ) where

import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SWF.Types

-- | /See:/ 'listWorkflowTypes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lwtrqNextPageToken'
--
-- * 'lwtrqReverseOrder'
--
-- * 'lwtrqName'
--
-- * 'lwtrqMaximumPageSize'
--
-- * 'lwtrqDomain'
--
-- * 'lwtrqRegistrationStatus'
data ListWorkflowTypes = ListWorkflowTypes'
    { _lwtrqNextPageToken      :: !(Maybe Text)
    , _lwtrqReverseOrder       :: !(Maybe Bool)
    , _lwtrqName               :: !(Maybe Text)
    , _lwtrqMaximumPageSize    :: !(Maybe Nat)
    , _lwtrqDomain             :: !Text
    , _lwtrqRegistrationStatus :: !RegistrationStatus
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListWorkflowTypes' smart constructor.
listWorkflowTypes :: Text -> RegistrationStatus -> ListWorkflowTypes
listWorkflowTypes pDomain_ pRegistrationStatus_ =
    ListWorkflowTypes'
    { _lwtrqNextPageToken = Nothing
    , _lwtrqReverseOrder = Nothing
    , _lwtrqName = Nothing
    , _lwtrqMaximumPageSize = Nothing
    , _lwtrqDomain = pDomain_
    , _lwtrqRegistrationStatus = pRegistrationStatus_
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
lwtrqNextPageToken :: Lens' ListWorkflowTypes (Maybe Text)
lwtrqNextPageToken = lens _lwtrqNextPageToken (\ s a -> s{_lwtrqNextPageToken = a});

-- | When set to @true@, returns the results in reverse order. By default the
-- results are returned in ascending alphabetical order of the @name@ of
-- the workflow types.
lwtrqReverseOrder :: Lens' ListWorkflowTypes (Maybe Bool)
lwtrqReverseOrder = lens _lwtrqReverseOrder (\ s a -> s{_lwtrqReverseOrder = a});

-- | If specified, lists the workflow type with this name.
lwtrqName :: Lens' ListWorkflowTypes (Maybe Text)
lwtrqName = lens _lwtrqName (\ s a -> s{_lwtrqName = a});

-- | The maximum number of results that will be returned per call.
-- @nextPageToken@ can be used to obtain futher pages of results. The
-- default is 100, which is the maximum allowed page size. You can,
-- however, specify a page size /smaller/ than 100.
--
-- This is an upper limit only; the actual number of results returned per
-- call may be fewer than the specified maximum.
lwtrqMaximumPageSize :: Lens' ListWorkflowTypes (Maybe Natural)
lwtrqMaximumPageSize = lens _lwtrqMaximumPageSize (\ s a -> s{_lwtrqMaximumPageSize = a}) . mapping _Nat;

-- | The name of the domain in which the workflow types have been registered.
lwtrqDomain :: Lens' ListWorkflowTypes Text
lwtrqDomain = lens _lwtrqDomain (\ s a -> s{_lwtrqDomain = a});

-- | Specifies the registration status of the workflow types to list.
lwtrqRegistrationStatus :: Lens' ListWorkflowTypes RegistrationStatus
lwtrqRegistrationStatus = lens _lwtrqRegistrationStatus (\ s a -> s{_lwtrqRegistrationStatus = a});

instance AWSPager ListWorkflowTypes where
        page rq rs
          | stop (rs ^. lwtrsNextPageToken) = Nothing
          | stop (rs ^. lwtrsTypeInfos) = Nothing
          | otherwise =
            Just $ rq &
              lwtrqNextPageToken .~ rs ^. lwtrsNextPageToken

instance AWSRequest ListWorkflowTypes where
        type Sv ListWorkflowTypes = SWF
        type Rs ListWorkflowTypes = ListWorkflowTypesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListWorkflowTypesResponse' <$>
                   (x .?> "nextPageToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "typeInfos" .!@ mempty))

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
              ["nextPageToken" .= _lwtrqNextPageToken,
               "reverseOrder" .= _lwtrqReverseOrder,
               "name" .= _lwtrqName,
               "maximumPageSize" .= _lwtrqMaximumPageSize,
               "domain" .= _lwtrqDomain,
               "registrationStatus" .= _lwtrqRegistrationStatus]

instance ToPath ListWorkflowTypes where
        toPath = const "/"

instance ToQuery ListWorkflowTypes where
        toQuery = const mempty

-- | Contains a paginated list of information structures about workflow
-- types.
--
-- /See:/ 'listWorkflowTypesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lwtrsNextPageToken'
--
-- * 'lwtrsStatus'
--
-- * 'lwtrsTypeInfos'
data ListWorkflowTypesResponse = ListWorkflowTypesResponse'
    { _lwtrsNextPageToken :: !(Maybe Text)
    , _lwtrsStatus        :: !Int
    , _lwtrsTypeInfos     :: ![WorkflowTypeInfo]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListWorkflowTypesResponse' smart constructor.
listWorkflowTypesResponse :: Int -> ListWorkflowTypesResponse
listWorkflowTypesResponse pStatus_ =
    ListWorkflowTypesResponse'
    { _lwtrsNextPageToken = Nothing
    , _lwtrsStatus = pStatus_
    , _lwtrsTypeInfos = mempty
    }

-- | If a @NextPageToken@ was returned by a previous call, there are more
-- results available. To retrieve the next page of results, make the call
-- again using the returned token in @nextPageToken@. Keep all other
-- arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be
-- returned in a single call.
lwtrsNextPageToken :: Lens' ListWorkflowTypesResponse (Maybe Text)
lwtrsNextPageToken = lens _lwtrsNextPageToken (\ s a -> s{_lwtrsNextPageToken = a});

-- | FIXME: Undocumented member.
lwtrsStatus :: Lens' ListWorkflowTypesResponse Int
lwtrsStatus = lens _lwtrsStatus (\ s a -> s{_lwtrsStatus = a});

-- | The list of workflow type information.
lwtrsTypeInfos :: Lens' ListWorkflowTypesResponse [WorkflowTypeInfo]
lwtrsTypeInfos = lens _lwtrsTypeInfos (\ s a -> s{_lwtrsTypeInfos = a});
