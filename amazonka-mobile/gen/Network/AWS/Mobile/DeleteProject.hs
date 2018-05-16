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
-- Module      : Network.AWS.Mobile.DeleteProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delets a project in AWS Mobile Hub.
--
--
module Network.AWS.Mobile.DeleteProject
    (
    -- * Creating a Request
      deleteProject
    , DeleteProject
    -- * Request Lenses
    , dpProjectId

    -- * Destructuring the Response
    , deleteProjectResponse
    , DeleteProjectResponse
    -- * Response Lenses
    , dprsDeletedResources
    , dprsOrphanedResources
    , dprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Mobile.Types
import Network.AWS.Mobile.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure used to request a project be deleted.
--
--
--
-- /See:/ 'deleteProject' smart constructor.
newtype DeleteProject = DeleteProject'
  { _dpProjectId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpProjectId' - Unique project identifier.
deleteProject
    :: Text -- ^ 'dpProjectId'
    -> DeleteProject
deleteProject pProjectId_ = DeleteProject' {_dpProjectId = pProjectId_}


-- | Unique project identifier.
dpProjectId :: Lens' DeleteProject Text
dpProjectId = lens _dpProjectId (\ s a -> s{_dpProjectId = a})

instance AWSRequest DeleteProject where
        type Rs DeleteProject = DeleteProjectResponse
        request = delete mobile
        response
          = receiveJSON
              (\ s h x ->
                 DeleteProjectResponse' <$>
                   (x .?> "deletedResources" .!@ mempty) <*>
                     (x .?> "orphanedResources" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DeleteProject where

instance NFData DeleteProject where

instance ToHeaders DeleteProject where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath DeleteProject where
        toPath DeleteProject'{..}
          = mconcat ["/projects/", toBS _dpProjectId]

instance ToQuery DeleteProject where
        toQuery = const mempty

-- | Result structure used in response to request to delete a project.
--
--
--
-- /See:/ 'deleteProjectResponse' smart constructor.
data DeleteProjectResponse = DeleteProjectResponse'
  { _dprsDeletedResources  :: !(Maybe [Resource])
  , _dprsOrphanedResources :: !(Maybe [Resource])
  , _dprsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dprsDeletedResources' - Resources which were deleted.
--
-- * 'dprsOrphanedResources' - Resources which were not deleted, due to a risk of losing potentially important data or files.
--
-- * 'dprsResponseStatus' - -- | The response status code.
deleteProjectResponse
    :: Int -- ^ 'dprsResponseStatus'
    -> DeleteProjectResponse
deleteProjectResponse pResponseStatus_ =
  DeleteProjectResponse'
    { _dprsDeletedResources = Nothing
    , _dprsOrphanedResources = Nothing
    , _dprsResponseStatus = pResponseStatus_
    }


-- | Resources which were deleted.
dprsDeletedResources :: Lens' DeleteProjectResponse [Resource]
dprsDeletedResources = lens _dprsDeletedResources (\ s a -> s{_dprsDeletedResources = a}) . _Default . _Coerce

-- | Resources which were not deleted, due to a risk of losing potentially important data or files.
dprsOrphanedResources :: Lens' DeleteProjectResponse [Resource]
dprsOrphanedResources = lens _dprsOrphanedResources (\ s a -> s{_dprsOrphanedResources = a}) . _Default . _Coerce

-- | -- | The response status code.
dprsResponseStatus :: Lens' DeleteProjectResponse Int
dprsResponseStatus = lens _dprsResponseStatus (\ s a -> s{_dprsResponseStatus = a})

instance NFData DeleteProjectResponse where
