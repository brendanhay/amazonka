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
-- Module      : Network.AWS.AlexaBusiness.ListSkillsStoreCategories
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all categories in the Alexa skill store.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkillsStoreCategories
    (
    -- * Creating a Request
      listSkillsStoreCategories
    , ListSkillsStoreCategories
    -- * Request Lenses
    , lsscNextToken
    , lsscMaxResults

    -- * Destructuring the Response
    , listSkillsStoreCategoriesResponse
    , ListSkillsStoreCategoriesResponse
    -- * Response Lenses
    , lsscrsCategoryList
    , lsscrsNextToken
    , lsscrsResponseStatus
    ) where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.AlexaBusiness.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSkillsStoreCategories' smart constructor.
data ListSkillsStoreCategories = ListSkillsStoreCategories'
  { _lsscNextToken  :: !(Maybe Text)
  , _lsscMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSkillsStoreCategories' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsscNextToken' - The tokens used for pagination.
--
-- * 'lsscMaxResults' - The maximum number of categories returned, per paginated calls.
listSkillsStoreCategories
    :: ListSkillsStoreCategories
listSkillsStoreCategories =
  ListSkillsStoreCategories'
    {_lsscNextToken = Nothing, _lsscMaxResults = Nothing}


-- | The tokens used for pagination.
lsscNextToken :: Lens' ListSkillsStoreCategories (Maybe Text)
lsscNextToken = lens _lsscNextToken (\ s a -> s{_lsscNextToken = a})

-- | The maximum number of categories returned, per paginated calls.
lsscMaxResults :: Lens' ListSkillsStoreCategories (Maybe Natural)
lsscMaxResults = lens _lsscMaxResults (\ s a -> s{_lsscMaxResults = a}) . mapping _Nat

instance AWSPager ListSkillsStoreCategories where
        page rq rs
          | stop (rs ^. lsscrsNextToken) = Nothing
          | stop (rs ^. lsscrsCategoryList) = Nothing
          | otherwise =
            Just $ rq & lsscNextToken .~ rs ^. lsscrsNextToken

instance AWSRequest ListSkillsStoreCategories where
        type Rs ListSkillsStoreCategories =
             ListSkillsStoreCategoriesResponse
        request = postJSON alexaBusiness
        response
          = receiveJSON
              (\ s h x ->
                 ListSkillsStoreCategoriesResponse' <$>
                   (x .?> "CategoryList" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListSkillsStoreCategories where

instance NFData ListSkillsStoreCategories where

instance ToHeaders ListSkillsStoreCategories where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AlexaForBusiness.ListSkillsStoreCategories" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSkillsStoreCategories where
        toJSON ListSkillsStoreCategories'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lsscNextToken,
                  ("MaxResults" .=) <$> _lsscMaxResults])

instance ToPath ListSkillsStoreCategories where
        toPath = const "/"

instance ToQuery ListSkillsStoreCategories where
        toQuery = const mempty

-- | /See:/ 'listSkillsStoreCategoriesResponse' smart constructor.
data ListSkillsStoreCategoriesResponse = ListSkillsStoreCategoriesResponse'
  { _lsscrsCategoryList   :: !(Maybe [Category])
  , _lsscrsNextToken      :: !(Maybe Text)
  , _lsscrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSkillsStoreCategoriesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsscrsCategoryList' - The list of categories.
--
-- * 'lsscrsNextToken' - The tokens used for pagination.
--
-- * 'lsscrsResponseStatus' - -- | The response status code.
listSkillsStoreCategoriesResponse
    :: Int -- ^ 'lsscrsResponseStatus'
    -> ListSkillsStoreCategoriesResponse
listSkillsStoreCategoriesResponse pResponseStatus_ =
  ListSkillsStoreCategoriesResponse'
    { _lsscrsCategoryList = Nothing
    , _lsscrsNextToken = Nothing
    , _lsscrsResponseStatus = pResponseStatus_
    }


-- | The list of categories.
lsscrsCategoryList :: Lens' ListSkillsStoreCategoriesResponse [Category]
lsscrsCategoryList = lens _lsscrsCategoryList (\ s a -> s{_lsscrsCategoryList = a}) . _Default . _Coerce

-- | The tokens used for pagination.
lsscrsNextToken :: Lens' ListSkillsStoreCategoriesResponse (Maybe Text)
lsscrsNextToken = lens _lsscrsNextToken (\ s a -> s{_lsscrsNextToken = a})

-- | -- | The response status code.
lsscrsResponseStatus :: Lens' ListSkillsStoreCategoriesResponse Int
lsscrsResponseStatus = lens _lsscrsResponseStatus (\ s a -> s{_lsscrsResponseStatus = a})

instance NFData ListSkillsStoreCategoriesResponse
         where
