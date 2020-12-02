{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all skills in the Alexa skill store by category.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListSkillsStoreSkillsByCategory
  ( -- * Creating a Request
    listSkillsStoreSkillsByCategory,
    ListSkillsStoreSkillsByCategory,

    -- * Request Lenses
    lsssbcNextToken,
    lsssbcMaxResults,
    lsssbcCategoryId,

    -- * Destructuring the Response
    listSkillsStoreSkillsByCategoryResponse,
    ListSkillsStoreSkillsByCategoryResponse,

    -- * Response Lenses
    lsssbcrsNextToken,
    lsssbcrsSkillsStoreSkills,
    lsssbcrsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listSkillsStoreSkillsByCategory' smart constructor.
data ListSkillsStoreSkillsByCategory = ListSkillsStoreSkillsByCategory'
  { _lsssbcNextToken ::
      !(Maybe Text),
    _lsssbcMaxResults ::
      !(Maybe Nat),
    _lsssbcCategoryId :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSkillsStoreSkillsByCategory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsssbcNextToken' - The tokens used for pagination.
--
-- * 'lsssbcMaxResults' - The maximum number of skills returned per paginated calls.
--
-- * 'lsssbcCategoryId' - The category ID for which the skills are being retrieved from the skill store.
listSkillsStoreSkillsByCategory ::
  -- | 'lsssbcCategoryId'
  Natural ->
  ListSkillsStoreSkillsByCategory
listSkillsStoreSkillsByCategory pCategoryId_ =
  ListSkillsStoreSkillsByCategory'
    { _lsssbcNextToken = Nothing,
      _lsssbcMaxResults = Nothing,
      _lsssbcCategoryId = _Nat # pCategoryId_
    }

-- | The tokens used for pagination.
lsssbcNextToken :: Lens' ListSkillsStoreSkillsByCategory (Maybe Text)
lsssbcNextToken = lens _lsssbcNextToken (\s a -> s {_lsssbcNextToken = a})

-- | The maximum number of skills returned per paginated calls.
lsssbcMaxResults :: Lens' ListSkillsStoreSkillsByCategory (Maybe Natural)
lsssbcMaxResults = lens _lsssbcMaxResults (\s a -> s {_lsssbcMaxResults = a}) . mapping _Nat

-- | The category ID for which the skills are being retrieved from the skill store.
lsssbcCategoryId :: Lens' ListSkillsStoreSkillsByCategory Natural
lsssbcCategoryId = lens _lsssbcCategoryId (\s a -> s {_lsssbcCategoryId = a}) . _Nat

instance AWSPager ListSkillsStoreSkillsByCategory where
  page rq rs
    | stop (rs ^. lsssbcrsNextToken) = Nothing
    | stop (rs ^. lsssbcrsSkillsStoreSkills) = Nothing
    | otherwise =
      Just $ rq & lsssbcNextToken .~ rs ^. lsssbcrsNextToken

instance AWSRequest ListSkillsStoreSkillsByCategory where
  type
    Rs ListSkillsStoreSkillsByCategory =
      ListSkillsStoreSkillsByCategoryResponse
  request = postJSON alexaBusiness
  response =
    receiveJSON
      ( \s h x ->
          ListSkillsStoreSkillsByCategoryResponse'
            <$> (x .?> "NextToken")
            <*> (x .?> "SkillsStoreSkills" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable ListSkillsStoreSkillsByCategory

instance NFData ListSkillsStoreSkillsByCategory

instance ToHeaders ListSkillsStoreSkillsByCategory where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.ListSkillsStoreSkillsByCategory" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListSkillsStoreSkillsByCategory where
  toJSON ListSkillsStoreSkillsByCategory' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lsssbcNextToken,
            ("MaxResults" .=) <$> _lsssbcMaxResults,
            Just ("CategoryId" .= _lsssbcCategoryId)
          ]
      )

instance ToPath ListSkillsStoreSkillsByCategory where
  toPath = const "/"

instance ToQuery ListSkillsStoreSkillsByCategory where
  toQuery = const mempty

-- | /See:/ 'listSkillsStoreSkillsByCategoryResponse' smart constructor.
data ListSkillsStoreSkillsByCategoryResponse = ListSkillsStoreSkillsByCategoryResponse'
  { _lsssbcrsNextToken ::
      !( Maybe
           Text
       ),
    _lsssbcrsSkillsStoreSkills ::
      !( Maybe
           [SkillsStoreSkill]
       ),
    _lsssbcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListSkillsStoreSkillsByCategoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsssbcrsNextToken' - The tokens used for pagination.
--
-- * 'lsssbcrsSkillsStoreSkills' - The skill store skills.
--
-- * 'lsssbcrsResponseStatus' - -- | The response status code.
listSkillsStoreSkillsByCategoryResponse ::
  -- | 'lsssbcrsResponseStatus'
  Int ->
  ListSkillsStoreSkillsByCategoryResponse
listSkillsStoreSkillsByCategoryResponse pResponseStatus_ =
  ListSkillsStoreSkillsByCategoryResponse'
    { _lsssbcrsNextToken =
        Nothing,
      _lsssbcrsSkillsStoreSkills = Nothing,
      _lsssbcrsResponseStatus = pResponseStatus_
    }

-- | The tokens used for pagination.
lsssbcrsNextToken :: Lens' ListSkillsStoreSkillsByCategoryResponse (Maybe Text)
lsssbcrsNextToken = lens _lsssbcrsNextToken (\s a -> s {_lsssbcrsNextToken = a})

-- | The skill store skills.
lsssbcrsSkillsStoreSkills :: Lens' ListSkillsStoreSkillsByCategoryResponse [SkillsStoreSkill]
lsssbcrsSkillsStoreSkills = lens _lsssbcrsSkillsStoreSkills (\s a -> s {_lsssbcrsSkillsStoreSkills = a}) . _Default . _Coerce

-- | -- | The response status code.
lsssbcrsResponseStatus :: Lens' ListSkillsStoreSkillsByCategoryResponse Int
lsssbcrsResponseStatus = lens _lsssbcrsResponseStatus (\s a -> s {_lsssbcrsResponseStatus = a})

instance NFData ListSkillsStoreSkillsByCategoryResponse
