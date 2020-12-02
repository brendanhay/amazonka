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
-- Module      : Network.AWS.CodeStar.CreateProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a project, including project resources. This action creates a project based on a submitted project request. A set of source code files and a toolchain template file can be included with the project request. If these are not provided, an empty project is created.
module Network.AWS.CodeStar.CreateProject
  ( -- * Creating a Request
    createProject,
    CreateProject,

    -- * Request Lenses
    cpSourceCode,
    cpToolchain,
    cpClientRequestToken,
    cpDescription,
    cpTags,
    cpName,
    cpId,

    -- * Destructuring the Response
    createProjectResponse,
    CreateProjectResponse,

    -- * Response Lenses
    cprsProjectTemplateId,
    cprsClientRequestToken,
    cprsResponseStatus,
    cprsId,
    cprsArn,
  )
where

import Network.AWS.CodeStar.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createProject' smart constructor.
data CreateProject = CreateProject'
  { _cpSourceCode ::
      !(Maybe [Code]),
    _cpToolchain :: !(Maybe Toolchain),
    _cpClientRequestToken :: !(Maybe Text),
    _cpDescription :: !(Maybe (Sensitive Text)),
    _cpTags :: !(Maybe (Map Text (Text))),
    _cpName :: !(Sensitive Text),
    _cpId :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpSourceCode' - A list of the Code objects submitted with the project request. If this parameter is specified, the request must also include the toolchain parameter.
--
-- * 'cpToolchain' - The name of the toolchain template file submitted with the project request. If this parameter is specified, the request must also include the sourceCode parameter.
--
-- * 'cpClientRequestToken' - A user- or system-generated token that identifies the entity that requested project creation. This token can be used to repeat the request.
--
-- * 'cpDescription' - The description of the project, if any.
--
-- * 'cpTags' - The tags created for the project.
--
-- * 'cpName' - The display name for the project to be created in AWS CodeStar.
--
-- * 'cpId' - The ID of the project to be created in AWS CodeStar.
createProject ::
  -- | 'cpName'
  Text ->
  -- | 'cpId'
  Text ->
  CreateProject
createProject pName_ pId_ =
  CreateProject'
    { _cpSourceCode = Nothing,
      _cpToolchain = Nothing,
      _cpClientRequestToken = Nothing,
      _cpDescription = Nothing,
      _cpTags = Nothing,
      _cpName = _Sensitive # pName_,
      _cpId = pId_
    }

-- | A list of the Code objects submitted with the project request. If this parameter is specified, the request must also include the toolchain parameter.
cpSourceCode :: Lens' CreateProject [Code]
cpSourceCode = lens _cpSourceCode (\s a -> s {_cpSourceCode = a}) . _Default . _Coerce

-- | The name of the toolchain template file submitted with the project request. If this parameter is specified, the request must also include the sourceCode parameter.
cpToolchain :: Lens' CreateProject (Maybe Toolchain)
cpToolchain = lens _cpToolchain (\s a -> s {_cpToolchain = a})

-- | A user- or system-generated token that identifies the entity that requested project creation. This token can be used to repeat the request.
cpClientRequestToken :: Lens' CreateProject (Maybe Text)
cpClientRequestToken = lens _cpClientRequestToken (\s a -> s {_cpClientRequestToken = a})

-- | The description of the project, if any.
cpDescription :: Lens' CreateProject (Maybe Text)
cpDescription = lens _cpDescription (\s a -> s {_cpDescription = a}) . mapping _Sensitive

-- | The tags created for the project.
cpTags :: Lens' CreateProject (HashMap Text (Text))
cpTags = lens _cpTags (\s a -> s {_cpTags = a}) . _Default . _Map

-- | The display name for the project to be created in AWS CodeStar.
cpName :: Lens' CreateProject Text
cpName = lens _cpName (\s a -> s {_cpName = a}) . _Sensitive

-- | The ID of the project to be created in AWS CodeStar.
cpId :: Lens' CreateProject Text
cpId = lens _cpId (\s a -> s {_cpId = a})

instance AWSRequest CreateProject where
  type Rs CreateProject = CreateProjectResponse
  request = postJSON codeStar
  response =
    receiveJSON
      ( \s h x ->
          CreateProjectResponse'
            <$> (x .?> "projectTemplateId")
            <*> (x .?> "clientRequestToken")
            <*> (pure (fromEnum s))
            <*> (x .:> "id")
            <*> (x .:> "arn")
      )

instance Hashable CreateProject

instance NFData CreateProject

instance ToHeaders CreateProject where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeStar_20170419.CreateProject" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateProject where
  toJSON CreateProject' {..} =
    object
      ( catMaybes
          [ ("sourceCode" .=) <$> _cpSourceCode,
            ("toolchain" .=) <$> _cpToolchain,
            ("clientRequestToken" .=) <$> _cpClientRequestToken,
            ("description" .=) <$> _cpDescription,
            ("tags" .=) <$> _cpTags,
            Just ("name" .= _cpName),
            Just ("id" .= _cpId)
          ]
      )

instance ToPath CreateProject where
  toPath = const "/"

instance ToQuery CreateProject where
  toQuery = const mempty

-- | /See:/ 'createProjectResponse' smart constructor.
data CreateProjectResponse = CreateProjectResponse'
  { _cprsProjectTemplateId ::
      !(Maybe Text),
    _cprsClientRequestToken :: !(Maybe Text),
    _cprsResponseStatus :: !Int,
    _cprsId :: !Text,
    _cprsArn :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateProjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cprsProjectTemplateId' - Reserved for future use.
--
-- * 'cprsClientRequestToken' - A user- or system-generated token that identifies the entity that requested project creation.
--
-- * 'cprsResponseStatus' - -- | The response status code.
--
-- * 'cprsId' - The ID of the project.
--
-- * 'cprsArn' - The Amazon Resource Name (ARN) of the created project.
createProjectResponse ::
  -- | 'cprsResponseStatus'
  Int ->
  -- | 'cprsId'
  Text ->
  -- | 'cprsArn'
  Text ->
  CreateProjectResponse
createProjectResponse pResponseStatus_ pId_ pArn_ =
  CreateProjectResponse'
    { _cprsProjectTemplateId = Nothing,
      _cprsClientRequestToken = Nothing,
      _cprsResponseStatus = pResponseStatus_,
      _cprsId = pId_,
      _cprsArn = pArn_
    }

-- | Reserved for future use.
cprsProjectTemplateId :: Lens' CreateProjectResponse (Maybe Text)
cprsProjectTemplateId = lens _cprsProjectTemplateId (\s a -> s {_cprsProjectTemplateId = a})

-- | A user- or system-generated token that identifies the entity that requested project creation.
cprsClientRequestToken :: Lens' CreateProjectResponse (Maybe Text)
cprsClientRequestToken = lens _cprsClientRequestToken (\s a -> s {_cprsClientRequestToken = a})

-- | -- | The response status code.
cprsResponseStatus :: Lens' CreateProjectResponse Int
cprsResponseStatus = lens _cprsResponseStatus (\s a -> s {_cprsResponseStatus = a})

-- | The ID of the project.
cprsId :: Lens' CreateProjectResponse Text
cprsId = lens _cprsId (\s a -> s {_cprsId = a})

-- | The Amazon Resource Name (ARN) of the created project.
cprsArn :: Lens' CreateProjectResponse Text
cprsArn = lens _cprsArn (\s a -> s {_cprsArn = a})

instance NFData CreateProjectResponse
