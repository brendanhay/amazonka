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
-- Module      : Network.AWS.CodeBuild.BatchGetBuilds
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about builds.
--
--
module Network.AWS.CodeBuild.BatchGetBuilds
    (
    -- * Creating a Request
      batchGetBuilds
    , BatchGetBuilds
    -- * Request Lenses
    , bgbIds

    -- * Destructuring the Response
    , batchGetBuildsResponse
    , BatchGetBuildsResponse
    -- * Response Lenses
    , bgbrsBuilds
    , bgbrsBuildsNotFound
    , bgbrsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetBuilds' smart constructor.
newtype BatchGetBuilds = BatchGetBuilds'
  { _bgbIds :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetBuilds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgbIds' - The IDs of the builds.
batchGetBuilds
    :: NonEmpty Text -- ^ 'bgbIds'
    -> BatchGetBuilds
batchGetBuilds pIds_ = BatchGetBuilds' {_bgbIds = _List1 # pIds_}


-- | The IDs of the builds.
bgbIds :: Lens' BatchGetBuilds (NonEmpty Text)
bgbIds = lens _bgbIds (\ s a -> s{_bgbIds = a}) . _List1

instance AWSRequest BatchGetBuilds where
        type Rs BatchGetBuilds = BatchGetBuildsResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetBuildsResponse' <$>
                   (x .?> "builds" .!@ mempty) <*>
                     (x .?> "buildsNotFound")
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetBuilds where

instance NFData BatchGetBuilds where

instance ToHeaders BatchGetBuilds where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.BatchGetBuilds" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetBuilds where
        toJSON BatchGetBuilds'{..}
          = object (catMaybes [Just ("ids" .= _bgbIds)])

instance ToPath BatchGetBuilds where
        toPath = const "/"

instance ToQuery BatchGetBuilds where
        toQuery = const mempty

-- | /See:/ 'batchGetBuildsResponse' smart constructor.
data BatchGetBuildsResponse = BatchGetBuildsResponse'
  { _bgbrsBuilds         :: !(Maybe [Build])
  , _bgbrsBuildsNotFound :: !(Maybe (List1 Text))
  , _bgbrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetBuildsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgbrsBuilds' - Information about the requested builds.
--
-- * 'bgbrsBuildsNotFound' - The IDs of builds for which information could not be found.
--
-- * 'bgbrsResponseStatus' - -- | The response status code.
batchGetBuildsResponse
    :: Int -- ^ 'bgbrsResponseStatus'
    -> BatchGetBuildsResponse
batchGetBuildsResponse pResponseStatus_ =
  BatchGetBuildsResponse'
    { _bgbrsBuilds = Nothing
    , _bgbrsBuildsNotFound = Nothing
    , _bgbrsResponseStatus = pResponseStatus_
    }


-- | Information about the requested builds.
bgbrsBuilds :: Lens' BatchGetBuildsResponse [Build]
bgbrsBuilds = lens _bgbrsBuilds (\ s a -> s{_bgbrsBuilds = a}) . _Default . _Coerce

-- | The IDs of builds for which information could not be found.
bgbrsBuildsNotFound :: Lens' BatchGetBuildsResponse (Maybe (NonEmpty Text))
bgbrsBuildsNotFound = lens _bgbrsBuildsNotFound (\ s a -> s{_bgbrsBuildsNotFound = a}) . mapping _List1

-- | -- | The response status code.
bgbrsResponseStatus :: Lens' BatchGetBuildsResponse Int
bgbrsResponseStatus = lens _bgbrsResponseStatus (\ s a -> s{_bgbrsResponseStatus = a})

instance NFData BatchGetBuildsResponse where
