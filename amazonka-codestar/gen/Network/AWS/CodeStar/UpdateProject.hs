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
-- Module      : Network.AWS.CodeStar.UpdateProject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a project in AWS CodeStar.
--
--
module Network.AWS.CodeStar.UpdateProject
    (
    -- * Creating a Request
      updateProject
    , UpdateProject
    -- * Request Lenses
    , upName
    , upDescription
    , upId

    -- * Destructuring the Response
    , updateProjectResponse
    , UpdateProjectResponse
    -- * Response Lenses
    , uprsResponseStatus
    ) where

import Network.AWS.CodeStar.Types
import Network.AWS.CodeStar.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateProject' smart constructor.
data UpdateProject = UpdateProject'
  { _upName        :: !(Maybe (Sensitive Text))
  , _upDescription :: !(Maybe (Sensitive Text))
  , _upId          :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upName' - The name of the project you want to update.
--
-- * 'upDescription' - The description of the project, if any.
--
-- * 'upId' - The ID of the project you want to update.
updateProject
    :: Text -- ^ 'upId'
    -> UpdateProject
updateProject pId_ =
  UpdateProject' {_upName = Nothing, _upDescription = Nothing, _upId = pId_}


-- | The name of the project you want to update.
upName :: Lens' UpdateProject (Maybe Text)
upName = lens _upName (\ s a -> s{_upName = a}) . mapping _Sensitive

-- | The description of the project, if any.
upDescription :: Lens' UpdateProject (Maybe Text)
upDescription = lens _upDescription (\ s a -> s{_upDescription = a}) . mapping _Sensitive

-- | The ID of the project you want to update.
upId :: Lens' UpdateProject Text
upId = lens _upId (\ s a -> s{_upId = a})

instance AWSRequest UpdateProject where
        type Rs UpdateProject = UpdateProjectResponse
        request = postJSON codeStar
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateProjectResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateProject where

instance NFData UpdateProject where

instance ToHeaders UpdateProject where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeStar_20170419.UpdateProject" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateProject where
        toJSON UpdateProject'{..}
          = object
              (catMaybes
                 [("name" .=) <$> _upName,
                  ("description" .=) <$> _upDescription,
                  Just ("id" .= _upId)])

instance ToPath UpdateProject where
        toPath = const "/"

instance ToQuery UpdateProject where
        toQuery = const mempty

-- | /See:/ 'updateProjectResponse' smart constructor.
newtype UpdateProjectResponse = UpdateProjectResponse'
  { _uprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uprsResponseStatus' - -- | The response status code.
updateProjectResponse
    :: Int -- ^ 'uprsResponseStatus'
    -> UpdateProjectResponse
updateProjectResponse pResponseStatus_ =
  UpdateProjectResponse' {_uprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uprsResponseStatus :: Lens' UpdateProjectResponse Int
uprsResponseStatus = lens _uprsResponseStatus (\ s a -> s{_uprsResponseStatus = a})

instance NFData UpdateProjectResponse where
