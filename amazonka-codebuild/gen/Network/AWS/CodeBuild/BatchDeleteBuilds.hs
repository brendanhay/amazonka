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
-- Module      : Network.AWS.CodeBuild.BatchDeleteBuilds
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more builds.
--
--
module Network.AWS.CodeBuild.BatchDeleteBuilds
    (
    -- * Creating a Request
      batchDeleteBuilds
    , BatchDeleteBuilds
    -- * Request Lenses
    , bdbIds

    -- * Destructuring the Response
    , batchDeleteBuildsResponse
    , BatchDeleteBuildsResponse
    -- * Response Lenses
    , bdbrsBuildsNotDeleted
    , bdbrsBuildsDeleted
    , bdbrsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDeleteBuilds' smart constructor.
newtype BatchDeleteBuilds = BatchDeleteBuilds'
  { _bdbIds :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteBuilds' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdbIds' - The IDs of the builds to delete.
batchDeleteBuilds
    :: NonEmpty Text -- ^ 'bdbIds'
    -> BatchDeleteBuilds
batchDeleteBuilds pIds_ = BatchDeleteBuilds' {_bdbIds = _List1 # pIds_}


-- | The IDs of the builds to delete.
bdbIds :: Lens' BatchDeleteBuilds (NonEmpty Text)
bdbIds = lens _bdbIds (\ s a -> s{_bdbIds = a}) . _List1

instance AWSRequest BatchDeleteBuilds where
        type Rs BatchDeleteBuilds = BatchDeleteBuildsResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 BatchDeleteBuildsResponse' <$>
                   (x .?> "buildsNotDeleted" .!@ mempty) <*>
                     (x .?> "buildsDeleted")
                     <*> (pure (fromEnum s)))

instance Hashable BatchDeleteBuilds where

instance NFData BatchDeleteBuilds where

instance ToHeaders BatchDeleteBuilds where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.BatchDeleteBuilds" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchDeleteBuilds where
        toJSON BatchDeleteBuilds'{..}
          = object (catMaybes [Just ("ids" .= _bdbIds)])

instance ToPath BatchDeleteBuilds where
        toPath = const "/"

instance ToQuery BatchDeleteBuilds where
        toQuery = const mempty

-- | /See:/ 'batchDeleteBuildsResponse' smart constructor.
data BatchDeleteBuildsResponse = BatchDeleteBuildsResponse'
  { _bdbrsBuildsNotDeleted :: !(Maybe [BuildNotDeleted])
  , _bdbrsBuildsDeleted    :: !(Maybe (List1 Text))
  , _bdbrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteBuildsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdbrsBuildsNotDeleted' - Information about any builds that could not be successfully deleted.
--
-- * 'bdbrsBuildsDeleted' - The IDs of the builds that were successfully deleted.
--
-- * 'bdbrsResponseStatus' - -- | The response status code.
batchDeleteBuildsResponse
    :: Int -- ^ 'bdbrsResponseStatus'
    -> BatchDeleteBuildsResponse
batchDeleteBuildsResponse pResponseStatus_ =
  BatchDeleteBuildsResponse'
    { _bdbrsBuildsNotDeleted = Nothing
    , _bdbrsBuildsDeleted = Nothing
    , _bdbrsResponseStatus = pResponseStatus_
    }


-- | Information about any builds that could not be successfully deleted.
bdbrsBuildsNotDeleted :: Lens' BatchDeleteBuildsResponse [BuildNotDeleted]
bdbrsBuildsNotDeleted = lens _bdbrsBuildsNotDeleted (\ s a -> s{_bdbrsBuildsNotDeleted = a}) . _Default . _Coerce

-- | The IDs of the builds that were successfully deleted.
bdbrsBuildsDeleted :: Lens' BatchDeleteBuildsResponse (Maybe (NonEmpty Text))
bdbrsBuildsDeleted = lens _bdbrsBuildsDeleted (\ s a -> s{_bdbrsBuildsDeleted = a}) . mapping _List1

-- | -- | The response status code.
bdbrsResponseStatus :: Lens' BatchDeleteBuildsResponse Int
bdbrsResponseStatus = lens _bdbrsResponseStatus (\ s a -> s{_bdbrsResponseStatus = a})

instance NFData BatchDeleteBuildsResponse where
