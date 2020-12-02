{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Rekognition Custom Labels project. A project is a logical grouping of resources (images, Labels, models) and operations (training, evaluation and detection).
--
--
-- This operation requires permissions to perform the @rekognition:CreateProject@ action.
module Network.AWS.Rekognition.CreateProject
  ( -- * Creating a Request
    createProject,
    CreateProject,

    -- * Request Lenses
    cpProjectName,

    -- * Destructuring the Response
    createProjectResponse,
    CreateProjectResponse,

    -- * Response Lenses
    cprsProjectARN,
    cprsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createProject' smart constructor.
newtype CreateProject = CreateProject' {_cpProjectName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpProjectName' - The name of the project to create.
createProject ::
  -- | 'cpProjectName'
  Text ->
  CreateProject
createProject pProjectName_ =
  CreateProject' {_cpProjectName = pProjectName_}

-- | The name of the project to create.
cpProjectName :: Lens' CreateProject Text
cpProjectName = lens _cpProjectName (\s a -> s {_cpProjectName = a})

instance AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request = postJSON rekognition
  response =
    receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            <$> (x .?> "ProjectArn") <*> (pure (fromEnum s))
      )

instance Hashable CreateProject

instance NFData CreateProject

instance ToHeaders CreateProject where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("RekognitionService.CreateProject" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateProject where
  toJSON CreateProject' {..} =
    object (catMaybes [Just ("ProjectName" .= _cpProjectName)])

instance ToPath CreateProject where
  toPath = const "/"

instance ToQuery CreateProject where
  toQuery = const mempty

-- | /See:/ 'createProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { _cprsProjectARN ::
      !(Maybe Text),
    _cprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsProjectARN' - The Amazon Resource Name (ARN) of the new project. You can use the ARN to configure IAM access to the project.
--
-- * 'cprsResponseStatus' - -- | The response status code.
createProjectResponse ::
  -- | 'cprsResponseStatus'
  Int ->
  CreateProjectResponse
createProjectResponse pResponseStatus_ =
  CreateProjectResponse'
    { _cprsProjectARN = Nothing,
      _cprsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the new project. You can use the ARN to configure IAM access to the project.
cprsProjectARN :: Lens' CreateProjectResponse (Maybe Text)
cprsProjectARN = lens _cprsProjectARN (\s a -> s {_cprsProjectARN = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreateProjectResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\s a -> s {_cprsResponseStatus = a})

instance NFData CreateProjectResponse
