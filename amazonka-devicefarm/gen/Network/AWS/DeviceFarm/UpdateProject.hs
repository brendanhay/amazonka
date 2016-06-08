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
-- Module      : Network.AWS.DeviceFarm.UpdateProject
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified project name, given the project ARN and a new name.
module Network.AWS.DeviceFarm.UpdateProject
    (
    -- * Creating a Request
      updateProject
    , UpdateProject
    -- * Request Lenses
    , upName
    , upArn

    -- * Destructuring the Response
    , updateProjectResponse
    , UpdateProjectResponse
    -- * Response Lenses
    , uprsProject
    , uprsResponseStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.DeviceFarm.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the update project operation.
--
-- /See:/ 'updateProject' smart constructor.
data UpdateProject = UpdateProject'
    { _upName :: !(Maybe Text)
    , _upArn  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upName'
--
-- * 'upArn'
updateProject
    :: Text -- ^ 'upArn'
    -> UpdateProject
updateProject pArn_ =
    UpdateProject'
    { _upName = Nothing
    , _upArn = pArn_
    }

-- | A string representing the new name of the project that you are updating.
upName :: Lens' UpdateProject (Maybe Text)
upName = lens _upName (\ s a -> s{_upName = a});

-- | The Amazon Resource Name (ARN) of the project whose name you wish to update.
upArn :: Lens' UpdateProject Text
upArn = lens _upArn (\ s a -> s{_upArn = a});

instance AWSRequest UpdateProject where
        type Rs UpdateProject = UpdateProjectResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateProjectResponse' <$>
                   (x .?> "project") <*> (pure (fromEnum s)))

instance Hashable UpdateProject

instance NFData UpdateProject

instance ToHeaders UpdateProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.UpdateProject" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateProject where
        toJSON UpdateProject'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _upName, Just ("arn" .= _upArn)])

instance ToPath UpdateProject where
        toPath = const "/"

instance ToQuery UpdateProject where
        toQuery = const mempty

-- | Represents the result of an update project request.
--
-- /See:/ 'updateProjectResponse' smart constructor.
data UpdateProjectResponse = UpdateProjectResponse'
    { _uprsProject        :: !(Maybe Project)
    , _uprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsProject'
--
-- * 'uprsResponseStatus'
updateProjectResponse
    :: Int -- ^ 'uprsResponseStatus'
    -> UpdateProjectResponse
updateProjectResponse pResponseStatus_ =
    UpdateProjectResponse'
    { _uprsProject = Nothing
    , _uprsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
uprsProject :: Lens' UpdateProjectResponse (Maybe Project)
uprsProject = lens _uprsProject (\ s a -> s{_uprsProject = a});

-- | The response status code.
uprsResponseStatus :: Lens' UpdateProjectResponse Int
uprsResponseStatus = lens _uprsResponseStatus (\ s a -> s{_uprsResponseStatus = a});

instance NFData UpdateProjectResponse
