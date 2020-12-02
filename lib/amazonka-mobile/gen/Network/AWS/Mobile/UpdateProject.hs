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
-- Module      : Network.AWS.Mobile.UpdateProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing project.
--
--
module Network.AWS.Mobile.UpdateProject
    (
    -- * Creating a Request
      updateProject
    , UpdateProject
    -- * Request Lenses
    , upContents
    , upProjectId

    -- * Destructuring the Response
    , updateProjectResponse
    , UpdateProjectResponse
    -- * Response Lenses
    , uprsDetails
    , uprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Mobile.Types
import Network.AWS.Mobile.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request structure used for requests to update project configuration.
--
--
--
-- /See:/ 'updateProject' smart constructor.
data UpdateProject = UpdateProject'
  { _upContents  :: !(Maybe ByteString)
  , _upProjectId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upContents' - ZIP or YAML file which contains project configuration to be updated. This should be the contents of the file downloaded from the URL provided in an export project operation.
--
-- * 'upProjectId' - Unique project identifier.
updateProject
    :: Text -- ^ 'upProjectId'
    -> UpdateProject
updateProject pProjectId_ =
  UpdateProject' {_upContents = Nothing, _upProjectId = pProjectId_}


-- | ZIP or YAML file which contains project configuration to be updated. This should be the contents of the file downloaded from the URL provided in an export project operation.
upContents :: Lens' UpdateProject (Maybe ByteString)
upContents = lens _upContents (\ s a -> s{_upContents = a})

-- | Unique project identifier.
upProjectId :: Lens' UpdateProject Text
upProjectId = lens _upProjectId (\ s a -> s{_upProjectId = a})

instance AWSRequest UpdateProject where
        type Rs UpdateProject = UpdateProjectResponse
        request = postBody mobile
        response
          = receiveJSON
              (\ s h x ->
                 UpdateProjectResponse' <$>
                   (x .?> "details") <*> (pure (fromEnum s)))

instance Hashable UpdateProject where

instance NFData UpdateProject where

instance ToBody UpdateProject where
        toBody = toBody . _upContents

instance ToHeaders UpdateProject where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToPath UpdateProject where
        toPath = const "/update"

instance ToQuery UpdateProject where
        toQuery UpdateProject'{..}
          = mconcat ["projectId" =: _upProjectId]

-- | Result structure used for requests to updated project configuration.
--
--
--
-- /See:/ 'updateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
  { _uprsDetails        :: !(Maybe ProjectDetails)
  , _uprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsDetails' - Detailed information about the updated AWS Mobile Hub project.
--
-- * 'uprsResponseStatus' - -- | The response status code.
updateProjectResponse
    :: Int -- ^ 'uprsResponseStatus'
    -> UpdateProjectResponse
updateProjectResponse pResponseStatus_ =
  UpdateProjectResponse'
    {_uprsDetails = Nothing, _uprsResponseStatus = pResponseStatus_}


-- | Detailed information about the updated AWS Mobile Hub project.
uprsDetails :: Lens' UpdateProjectResponse (Maybe ProjectDetails)
uprsDetails = lens _uprsDetails (\ s a -> s{_uprsDetails = a})

-- | -- | The response status code.
uprsResponseStatus :: Lens' UpdateProjectResponse Int
uprsResponseStatus = lens _uprsResponseStatus (\ s a -> s{_uprsResponseStatus = a})

instance NFData UpdateProjectResponse where
