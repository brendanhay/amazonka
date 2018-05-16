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
-- Module      : Network.AWS.MechanicalTurk.ListHITsForQualificationType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListHITsForQualificationType@ operation returns the HITs that use the given Qualification type for a Qualification requirement. The operation returns HITs of any status, except for HITs that have been deleted with the @DeleteHIT@ operation or that have been auto-deleted.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListHITsForQualificationType
    (
    -- * Creating a Request
      listHITsForQualificationType
    , ListHITsForQualificationType
    -- * Request Lenses
    , lhitfqtNextToken
    , lhitfqtMaxResults
    , lhitfqtQualificationTypeId

    -- * Destructuring the Response
    , listHITsForQualificationTypeResponse
    , ListHITsForQualificationTypeResponse
    -- * Response Lenses
    , lhitfqtrsNextToken
    , lhitfqtrsNumResults
    , lhitfqtrsHITs
    , lhitfqtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types
import Network.AWS.MechanicalTurk.Types.Product
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listHITsForQualificationType' smart constructor.
data ListHITsForQualificationType = ListHITsForQualificationType'
  { _lhitfqtNextToken           :: !(Maybe Text)
  , _lhitfqtMaxResults          :: !(Maybe Nat)
  , _lhitfqtQualificationTypeId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHITsForQualificationType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhitfqtNextToken' - Pagination Token
--
-- * 'lhitfqtMaxResults' - Limit the number of results returned.
--
-- * 'lhitfqtQualificationTypeId' - The ID of the Qualification type to use when querying HITs.
listHITsForQualificationType
    :: Text -- ^ 'lhitfqtQualificationTypeId'
    -> ListHITsForQualificationType
listHITsForQualificationType pQualificationTypeId_ =
  ListHITsForQualificationType'
    { _lhitfqtNextToken = Nothing
    , _lhitfqtMaxResults = Nothing
    , _lhitfqtQualificationTypeId = pQualificationTypeId_
    }


-- | Pagination Token
lhitfqtNextToken :: Lens' ListHITsForQualificationType (Maybe Text)
lhitfqtNextToken = lens _lhitfqtNextToken (\ s a -> s{_lhitfqtNextToken = a})

-- | Limit the number of results returned.
lhitfqtMaxResults :: Lens' ListHITsForQualificationType (Maybe Natural)
lhitfqtMaxResults = lens _lhitfqtMaxResults (\ s a -> s{_lhitfqtMaxResults = a}) . mapping _Nat

-- | The ID of the Qualification type to use when querying HITs.
lhitfqtQualificationTypeId :: Lens' ListHITsForQualificationType Text
lhitfqtQualificationTypeId = lens _lhitfqtQualificationTypeId (\ s a -> s{_lhitfqtQualificationTypeId = a})

instance AWSPager ListHITsForQualificationType where
        page rq rs
          | stop (rs ^. lhitfqtrsNextToken) = Nothing
          | stop (rs ^. lhitfqtrsHITs) = Nothing
          | otherwise =
            Just $ rq &
              lhitfqtNextToken .~ rs ^. lhitfqtrsNextToken

instance AWSRequest ListHITsForQualificationType
         where
        type Rs ListHITsForQualificationType =
             ListHITsForQualificationTypeResponse
        request = postJSON mechanicalTurk
        response
          = receiveJSON
              (\ s h x ->
                 ListHITsForQualificationTypeResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "NumResults") <*>
                     (x .?> "HITs" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListHITsForQualificationType where

instance NFData ListHITsForQualificationType where

instance ToHeaders ListHITsForQualificationType where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("MTurkRequesterServiceV20170117.ListHITsForQualificationType"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListHITsForQualificationType where
        toJSON ListHITsForQualificationType'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lhitfqtNextToken,
                  ("MaxResults" .=) <$> _lhitfqtMaxResults,
                  Just
                    ("QualificationTypeId" .=
                       _lhitfqtQualificationTypeId)])

instance ToPath ListHITsForQualificationType where
        toPath = const "/"

instance ToQuery ListHITsForQualificationType where
        toQuery = const mempty

-- | /See:/ 'listHITsForQualificationTypeResponse' smart constructor.
data ListHITsForQualificationTypeResponse = ListHITsForQualificationTypeResponse'
  { _lhitfqtrsNextToken      :: !(Maybe Text)
  , _lhitfqtrsNumResults     :: !(Maybe Int)
  , _lhitfqtrsHITs           :: !(Maybe [HIT])
  , _lhitfqtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListHITsForQualificationTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lhitfqtrsNextToken' - Undocumented member.
--
-- * 'lhitfqtrsNumResults' - The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
--
-- * 'lhitfqtrsHITs' - The list of HIT elements returned by the query.
--
-- * 'lhitfqtrsResponseStatus' - -- | The response status code.
listHITsForQualificationTypeResponse
    :: Int -- ^ 'lhitfqtrsResponseStatus'
    -> ListHITsForQualificationTypeResponse
listHITsForQualificationTypeResponse pResponseStatus_ =
  ListHITsForQualificationTypeResponse'
    { _lhitfqtrsNextToken = Nothing
    , _lhitfqtrsNumResults = Nothing
    , _lhitfqtrsHITs = Nothing
    , _lhitfqtrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
lhitfqtrsNextToken :: Lens' ListHITsForQualificationTypeResponse (Maybe Text)
lhitfqtrsNextToken = lens _lhitfqtrsNextToken (\ s a -> s{_lhitfqtrsNextToken = a})

-- | The number of HITs on this page in the filtered results list, equivalent to the number of HITs being returned by this call.
lhitfqtrsNumResults :: Lens' ListHITsForQualificationTypeResponse (Maybe Int)
lhitfqtrsNumResults = lens _lhitfqtrsNumResults (\ s a -> s{_lhitfqtrsNumResults = a})

-- | The list of HIT elements returned by the query.
lhitfqtrsHITs :: Lens' ListHITsForQualificationTypeResponse [HIT]
lhitfqtrsHITs = lens _lhitfqtrsHITs (\ s a -> s{_lhitfqtrsHITs = a}) . _Default . _Coerce

-- | -- | The response status code.
lhitfqtrsResponseStatus :: Lens' ListHITsForQualificationTypeResponse Int
lhitfqtrsResponseStatus = lens _lhitfqtrsResponseStatus (\ s a -> s{_lhitfqtrsResponseStatus = a})

instance NFData ListHITsForQualificationTypeResponse
         where
