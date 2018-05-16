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
-- Module      : Network.AWS.CodeStar.ListTagsForProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the tags for a project.
--
--
module Network.AWS.CodeStar.ListTagsForProject
    (
    -- * Creating a Request
      listTagsForProject
    , ListTagsForProject
    -- * Request Lenses
    , ltfpNextToken
    , ltfpMaxResults
    , ltfpId

    -- * Destructuring the Response
    , listTagsForProjectResponse
    , ListTagsForProjectResponse
    -- * Response Lenses
    , ltfprsNextToken
    , ltfprsTags
    , ltfprsResponseStatus
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTagsForProject' smart constructor.
data ListTagsForProject = ListTagsForProject'
  { _ltfpNextToken  :: !(Maybe Text)
  , _ltfpMaxResults :: !(Maybe Nat)
  , _ltfpId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfpNextToken' - Reserved for future use.
--
-- * 'ltfpMaxResults' - Reserved for future use.
--
-- * 'ltfpId' - The ID of the project to get tags for.
listTagsForProject
    :: Text -- ^ 'ltfpId'
    -> ListTagsForProject
listTagsForProject pId_ =
  ListTagsForProject'
    {_ltfpNextToken = Nothing, _ltfpMaxResults = Nothing, _ltfpId = pId_}


-- | Reserved for future use.
ltfpNextToken :: Lens' ListTagsForProject (Maybe Text)
ltfpNextToken = lens _ltfpNextToken (\ s a -> s{_ltfpNextToken = a})

-- | Reserved for future use.
ltfpMaxResults :: Lens' ListTagsForProject (Maybe Natural)
ltfpMaxResults = lens _ltfpMaxResults (\ s a -> s{_ltfpMaxResults = a}) . mapping _Nat

-- | The ID of the project to get tags for.
ltfpId :: Lens' ListTagsForProject Text
ltfpId = lens _ltfpId (\ s a -> s{_ltfpId = a})

instance AWSRequest ListTagsForProject where
        type Rs ListTagsForProject =
             ListTagsForProjectResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 ListTagsForProjectResponse' <$>
                   (x .?> "nextToken") <*> (x .?> "tags" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListTagsForProject where

instance NFData ListTagsForProject where

instance ToHeaders ListTagsForProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.ListTagsForProject" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTagsForProject where
        toJSON ListTagsForProject'{..}
          = object
              (catMaybes
                 [("nextToken" .=) <$> _ltfpNextToken,
                  ("maxResults" .=) <$> _ltfpMaxResults,
                  Just ("id" .= _ltfpId)])

instance ToPath ListTagsForProject where
        toPath = const "/"

instance ToQuery ListTagsForProject where
        toQuery = const mempty

-- | /See:/ 'listTagsForProjectResponse' smart constructor.
data ListTagsForProjectResponse = ListTagsForProjectResponse'
  { _ltfprsNextToken      :: !(Maybe Text)
  , _ltfprsTags           :: !(Maybe (Map Text Text))
  , _ltfprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfprsNextToken' - Reserved for future use.
--
-- * 'ltfprsTags' - The tags for the project.
--
-- * 'ltfprsResponseStatus' - -- | The response status code.
listTagsForProjectResponse
    :: Int -- ^ 'ltfprsResponseStatus'
    -> ListTagsForProjectResponse
listTagsForProjectResponse pResponseStatus_ =
  ListTagsForProjectResponse'
    { _ltfprsNextToken = Nothing
    , _ltfprsTags = Nothing
    , _ltfprsResponseStatus = pResponseStatus_
    }


-- | Reserved for future use.
ltfprsNextToken :: Lens' ListTagsForProjectResponse (Maybe Text)
ltfprsNextToken = lens _ltfprsNextToken (\ s a -> s{_ltfprsNextToken = a})

-- | The tags for the project.
ltfprsTags :: Lens' ListTagsForProjectResponse (HashMap Text Text)
ltfprsTags = lens _ltfprsTags (\ s a -> s{_ltfprsTags = a}) . _Default . _Map

-- | -- | The response status code.
ltfprsResponseStatus :: Lens' ListTagsForProjectResponse Int
ltfprsResponseStatus = lens _ltfprsResponseStatus (\ s a -> s{_ltfprsResponseStatus = a})

instance NFData ListTagsForProjectResponse where
