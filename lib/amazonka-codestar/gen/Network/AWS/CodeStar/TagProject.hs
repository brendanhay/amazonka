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
-- Module      : Network.AWS.CodeStar.TagProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds tags to a project.
--
--
module Network.AWS.CodeStar.TagProject
    (
    -- * Creating a Request
      tagProject
    , TagProject
    -- * Request Lenses
    , tpId
    , tpTags

    -- * Destructuring the Response
    , tagProjectResponse
    , TagProjectResponse
    -- * Response Lenses
    , tprsTags
    , tprsResponseStatus
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'tagProject' smart constructor.
data TagProject = TagProject'
  { _tpId   :: !Text
  , _tpTags :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tpId' - The ID of the project you want to add a tag to.
--
-- * 'tpTags' - The tags you want to add to the project.
tagProject
    :: Text -- ^ 'tpId'
    -> TagProject
tagProject pId_ = TagProject' {_tpId = pId_, _tpTags = mempty}


-- | The ID of the project you want to add a tag to.
tpId :: Lens' TagProject Text
tpId = lens _tpId (\ s a -> s{_tpId = a})

-- | The tags you want to add to the project.
tpTags :: Lens' TagProject (HashMap Text Text)
tpTags = lens _tpTags (\ s a -> s{_tpTags = a}) . _Map

instance AWSRequest TagProject where
        type Rs TagProject = TagProjectResponse
        request = postJSON codeStar
        response
          = receiveJSON
              (\ s h x ->
                 TagProjectResponse' <$>
                   (x .?> "tags" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable TagProject where

instance NFData TagProject where

instance ToHeaders TagProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.TagProject" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON TagProject where
        toJSON TagProject'{..}
          = object
              (catMaybes
                 [Just ("id" .= _tpId), Just ("tags" .= _tpTags)])

instance ToPath TagProject where
        toPath = const "/"

instance ToQuery TagProject where
        toQuery = const mempty

-- | /See:/ 'tagProjectResponse' smart constructor.
data TagProjectResponse = TagProjectResponse'
  { _tprsTags           :: !(Maybe (Map Text Text))
  , _tprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'TagProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tprsTags' - The tags for the project.
--
-- * 'tprsResponseStatus' - -- | The response status code.
tagProjectResponse
    :: Int -- ^ 'tprsResponseStatus'
    -> TagProjectResponse
tagProjectResponse pResponseStatus_ =
  TagProjectResponse'
    {_tprsTags = Nothing, _tprsResponseStatus = pResponseStatus_}


-- | The tags for the project.
tprsTags :: Lens' TagProjectResponse (HashMap Text Text)
tprsTags = lens _tprsTags (\ s a -> s{_tprsTags = a}) . _Default . _Map

-- | -- | The response status code.
tprsResponseStatus :: Lens' TagProjectResponse Int
tprsResponseStatus = lens _tprsResponseStatus (\ s a -> s{_tprsResponseStatus = a})

instance NFData TagProjectResponse where
