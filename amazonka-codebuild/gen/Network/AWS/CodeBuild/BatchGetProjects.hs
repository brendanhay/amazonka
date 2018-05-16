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
-- Module      : Network.AWS.CodeBuild.BatchGetProjects
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about build projects.
--
--
module Network.AWS.CodeBuild.BatchGetProjects
    (
    -- * Creating a Request
      batchGetProjects
    , BatchGetProjects
    -- * Request Lenses
    , bgpNames

    -- * Destructuring the Response
    , batchGetProjectsResponse
    , BatchGetProjectsResponse
    -- * Response Lenses
    , bgprsProjectsNotFound
    , bgprsProjects
    , bgprsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchGetProjects' smart constructor.
newtype BatchGetProjects = BatchGetProjects'
  { _bgpNames :: List1 Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetProjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgpNames' - The names of the build projects.
batchGetProjects
    :: NonEmpty Text -- ^ 'bgpNames'
    -> BatchGetProjects
batchGetProjects pNames_ = BatchGetProjects' {_bgpNames = _List1 # pNames_}


-- | The names of the build projects.
bgpNames :: Lens' BatchGetProjects (NonEmpty Text)
bgpNames = lens _bgpNames (\ s a -> s{_bgpNames = a}) . _List1

instance AWSRequest BatchGetProjects where
        type Rs BatchGetProjects = BatchGetProjectsResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 BatchGetProjectsResponse' <$>
                   (x .?> "projectsNotFound") <*>
                     (x .?> "projects" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable BatchGetProjects where

instance NFData BatchGetProjects where

instance ToHeaders BatchGetProjects where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.BatchGetProjects" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON BatchGetProjects where
        toJSON BatchGetProjects'{..}
          = object (catMaybes [Just ("names" .= _bgpNames)])

instance ToPath BatchGetProjects where
        toPath = const "/"

instance ToQuery BatchGetProjects where
        toQuery = const mempty

-- | /See:/ 'batchGetProjectsResponse' smart constructor.
data BatchGetProjectsResponse = BatchGetProjectsResponse'
  { _bgprsProjectsNotFound :: !(Maybe (List1 Text))
  , _bgprsProjects         :: !(Maybe [Project])
  , _bgprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchGetProjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bgprsProjectsNotFound' - The names of build projects for which information could not be found.
--
-- * 'bgprsProjects' - Information about the requested build projects.
--
-- * 'bgprsResponseStatus' - -- | The response status code.
batchGetProjectsResponse
    :: Int -- ^ 'bgprsResponseStatus'
    -> BatchGetProjectsResponse
batchGetProjectsResponse pResponseStatus_ =
  BatchGetProjectsResponse'
    { _bgprsProjectsNotFound = Nothing
    , _bgprsProjects = Nothing
    , _bgprsResponseStatus = pResponseStatus_
    }


-- | The names of build projects for which information could not be found.
bgprsProjectsNotFound :: Lens' BatchGetProjectsResponse (Maybe (NonEmpty Text))
bgprsProjectsNotFound = lens _bgprsProjectsNotFound (\ s a -> s{_bgprsProjectsNotFound = a}) . mapping _List1

-- | Information about the requested build projects.
bgprsProjects :: Lens' BatchGetProjectsResponse [Project]
bgprsProjects = lens _bgprsProjects (\ s a -> s{_bgprsProjects = a}) . _Default . _Coerce

-- | -- | The response status code.
bgprsResponseStatus :: Lens' BatchGetProjectsResponse Int
bgprsResponseStatus = lens _bgprsResponseStatus (\ s a -> s{_bgprsResponseStatus = a})

instance NFData BatchGetProjectsResponse where
