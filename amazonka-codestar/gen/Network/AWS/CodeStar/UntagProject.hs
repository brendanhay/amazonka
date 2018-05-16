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
-- Module      : Network.AWS.CodeStar.UntagProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes tags from a project.
--
--
module Network.AWS.CodeStar.UntagProject
    (
    -- * Creating a Request
      untagProject
    , UntagProject
    -- * Request Lenses
    , uId
    , uTags

    -- * Destructuring the Response
    , untagProjectResponse
    , UntagProjectResponse
    -- * Response Lenses
    , ursResponseStatus
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'untagProject' smart constructor.
data UntagProject = UntagProject'
  { _uId   :: !Text
  , _uTags :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uId' - The ID of the project to remove tags from.
--
-- * 'uTags' - The tags to remove from the project.
untagProject
    :: Text -- ^ 'uId'
    -> UntagProject
untagProject pId_ = UntagProject' {_uId = pId_, _uTags = mempty}


-- | The ID of the project to remove tags from.
uId :: Lens' UntagProject Text
uId = lens _uId (\ s a -> s{_uId = a})

-- | The tags to remove from the project.
uTags :: Lens' UntagProject [Text]
uTags = lens _uTags (\ s a -> s{_uTags = a}) . _Coerce

instance AWSRequest UntagProject where
        type Rs UntagProject = UntagProjectResponse
        request = postJSON codeStar
        response
          = receiveEmpty
              (\ s h x ->
                 UntagProjectResponse' <$> (pure (fromEnum s)))

instance Hashable UntagProject where

instance NFData UntagProject where

instance ToHeaders UntagProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.UntagProject" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UntagProject where
        toJSON UntagProject'{..}
          = object
              (catMaybes
                 [Just ("id" .= _uId), Just ("tags" .= _uTags)])

instance ToPath UntagProject where
        toPath = const "/"

instance ToQuery UntagProject where
        toQuery = const mempty

-- | /See:/ 'untagProjectResponse' smart constructor.
newtype UntagProjectResponse = UntagProjectResponse'
  { _ursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UntagProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ursResponseStatus' - -- | The response status code.
untagProjectResponse
    :: Int -- ^ 'ursResponseStatus'
    -> UntagProjectResponse
untagProjectResponse pResponseStatus_ =
  UntagProjectResponse' {_ursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ursResponseStatus :: Lens' UntagProjectResponse Int
ursResponseStatus = lens _ursResponseStatus (\ s a -> s{_ursResponseStatus = a})

instance NFData UntagProjectResponse where
