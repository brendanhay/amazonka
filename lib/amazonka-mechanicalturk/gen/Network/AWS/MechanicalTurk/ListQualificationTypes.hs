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
-- Module      : Network.AWS.MechanicalTurk.ListQualificationTypes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListQualificationTypes@ operation returns a list of Qualification types, filtered by an optional search term.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListQualificationTypes
    (
    -- * Creating a Request
      listQualificationTypes
    , ListQualificationTypes
    -- * Request Lenses
    , lqtMustBeOwnedByCaller
    , lqtNextToken
    , lqtQuery
    , lqtMaxResults
    , lqtMustBeRequestable

    -- * Destructuring the Response
    , listQualificationTypesResponse
    , ListQualificationTypesResponse
    -- * Response Lenses
    , lqtrsQualificationTypes
    , lqtrsNextToken
    , lqtrsNumResults
    , lqtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listQualificationTypes' smart constructor.
data ListQualificationTypes = ListQualificationTypes'
  { _lqtMustBeOwnedByCaller :: !(Maybe Bool)
  , _lqtNextToken           :: !(Maybe Text)
  , _lqtQuery               :: !(Maybe Text)
  , _lqtMaxResults          :: !(Maybe Nat)
  , _lqtMustBeRequestable   :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQualificationTypes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqtMustBeOwnedByCaller' - Specifies that only Qualification types that the Requester created are returned. If false, the operation returns all Qualification types.
--
-- * 'lqtNextToken' - Undocumented member.
--
-- * 'lqtQuery' - A text query against all of the searchable attributes of Qualification types.
--
-- * 'lqtMaxResults' - The maximum number of results to return in a single call.
--
-- * 'lqtMustBeRequestable' - Specifies that only Qualification types that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test, are returned as results of the search. Some Qualification types, such as those assigned automatically by the system, cannot be requested directly by users. If false, all Qualification types, including those managed by the system, are considered. Valid values are True | False.
listQualificationTypes
    :: Bool -- ^ 'lqtMustBeRequestable'
    -> ListQualificationTypes
listQualificationTypes pMustBeRequestable_ =
  ListQualificationTypes'
    { _lqtMustBeOwnedByCaller = Nothing
    , _lqtNextToken = Nothing
    , _lqtQuery = Nothing
    , _lqtMaxResults = Nothing
    , _lqtMustBeRequestable = pMustBeRequestable_
    }


-- | Specifies that only Qualification types that the Requester created are returned. If false, the operation returns all Qualification types.
lqtMustBeOwnedByCaller :: Lens' ListQualificationTypes (Maybe Bool)
lqtMustBeOwnedByCaller = lens _lqtMustBeOwnedByCaller (\ s a -> s{_lqtMustBeOwnedByCaller = a})

-- | Undocumented member.
lqtNextToken :: Lens' ListQualificationTypes (Maybe Text)
lqtNextToken = lens _lqtNextToken (\ s a -> s{_lqtNextToken = a})

-- | A text query against all of the searchable attributes of Qualification types.
lqtQuery :: Lens' ListQualificationTypes (Maybe Text)
lqtQuery = lens _lqtQuery (\ s a -> s{_lqtQuery = a})

-- | The maximum number of results to return in a single call.
lqtMaxResults :: Lens' ListQualificationTypes (Maybe Natural)
lqtMaxResults = lens _lqtMaxResults (\ s a -> s{_lqtMaxResults = a}) . mapping _Nat

-- | Specifies that only Qualification types that a user can request through the Amazon Mechanical Turk web site, such as by taking a Qualification test, are returned as results of the search. Some Qualification types, such as those assigned automatically by the system, cannot be requested directly by users. If false, all Qualification types, including those managed by the system, are considered. Valid values are True | False.
lqtMustBeRequestable :: Lens' ListQualificationTypes Bool
lqtMustBeRequestable = lens _lqtMustBeRequestable (\ s a -> s{_lqtMustBeRequestable = a})

instance AWSPager ListQualificationTypes where
        page rq rs
          | stop (rs ^. lqtrsNextToken) = Nothing
          | stop (rs ^. lqtrsQualificationTypes) = Nothing
          | otherwise =
            Just $ rq & lqtNextToken .~ rs ^. lqtrsNextToken

instance AWSRequest ListQualificationTypes where
        type Rs ListQualificationTypes =
             ListQualificationTypesResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 ListQualificationTypesResponse' <$>
                   (x .?> "QualificationTypes" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (x .?> "NumResults")
                     <*> (pure (fromEnum s)))

instance Hashable ListQualificationTypes where

instance NFData ListQualificationTypes where

instance ToHeaders ListQualificationTypes where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.ListQualificationTypes"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListQualificationTypes where
        toJSON ListQualificationTypes'{..}
          = object
              (catMaybes
                 [("MustBeOwnedByCaller" .=) <$>
                    _lqtMustBeOwnedByCaller,
                  ("NextToken" .=) <$> _lqtNextToken,
                  ("Query" .=) <$> _lqtQuery,
                  ("MaxResults" .=) <$> _lqtMaxResults,
                  Just ("MustBeRequestable" .= _lqtMustBeRequestable)])

instance ToPath ListQualificationTypes where
        toPath = const "/"

instance ToQuery ListQualificationTypes where
        toQuery = const mempty

-- | /See:/ 'listQualificationTypesResponse' smart constructor.
data ListQualificationTypesResponse = ListQualificationTypesResponse'
  { _lqtrsQualificationTypes :: !(Maybe [QualificationType])
  , _lqtrsNextToken          :: !(Maybe Text)
  , _lqtrsNumResults         :: !(Maybe Int)
  , _lqtrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListQualificationTypesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lqtrsQualificationTypes' - The list of QualificationType elements returned by the query.
--
-- * 'lqtrsNextToken' - Undocumented member.
--
-- * 'lqtrsNumResults' - The number of Qualification types on this page in the filtered results list, equivalent to the number of types this operation returns.
--
-- * 'lqtrsResponseStatus' - -- | The response status code.
listQualificationTypesResponse
    :: Int -- ^ 'lqtrsResponseStatus'
    -> ListQualificationTypesResponse
listQualificationTypesResponse pResponseStatus_ =
  ListQualificationTypesResponse'
    { _lqtrsQualificationTypes = Nothing
    , _lqtrsNextToken = Nothing
    , _lqtrsNumResults = Nothing
    , _lqtrsResponseStatus = pResponseStatus_
    }


-- | The list of QualificationType elements returned by the query.
lqtrsQualificationTypes :: Lens' ListQualificationTypesResponse [QualificationType]
lqtrsQualificationTypes = lens _lqtrsQualificationTypes (\ s a -> s{_lqtrsQualificationTypes = a}) . _Default . _Coerce

-- | Undocumented member.
lqtrsNextToken :: Lens' ListQualificationTypesResponse (Maybe Text)
lqtrsNextToken = lens _lqtrsNextToken (\ s a -> s{_lqtrsNextToken = a})

-- | The number of Qualification types on this page in the filtered results list, equivalent to the number of types this operation returns.
lqtrsNumResults :: Lens' ListQualificationTypesResponse (Maybe Int)
lqtrsNumResults = lens _lqtrsNumResults (\ s a -> s{_lqtrsNumResults = a})

-- | -- | The response status code.
lqtrsResponseStatus :: Lens' ListQualificationTypesResponse Int
lqtrsResponseStatus = lens _lqtrsResponseStatus (\ s a -> s{_lqtrsResponseStatus = a})

instance NFData ListQualificationTypesResponse where
