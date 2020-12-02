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
-- Module      : Network.AWS.CodeBuild.CreateWebhook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For an existing AWS CodeBuild build project that has its source code stored in a GitHub or Bitbucket repository, enables AWS CodeBuild to start rebuilding the source code every time a code change is pushed to the repository.
--
--
-- /Important:/ If you enable webhooks for an AWS CodeBuild project, and the project is used as a build step in AWS CodePipeline, then two identical builds are created for each commit. One build is triggered through webhooks, and one through AWS CodePipeline. Because billing is on a per-build basis, you are billed for both builds. Therefore, if you are using AWS CodePipeline, we recommend that you disable webhooks in AWS CodeBuild. In the AWS CodeBuild console, clear the Webhook box. For more information, see step 5 in <https://docs.aws.amazon.com/codebuild/latest/userguide/change-project.html#change-project-console Change a Build Project's Settings> .
module Network.AWS.CodeBuild.CreateWebhook
  ( -- * Creating a Request
    createWebhook,
    CreateWebhook,

    -- * Request Lenses
    cwBranchFilter,
    cwFilterGroups,
    cwBuildType,
    cwProjectName,

    -- * Destructuring the Response
    createWebhookResponse,
    CreateWebhookResponse,

    -- * Response Lenses
    cwrsWebhook,
    cwrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createWebhook' smart constructor.
data CreateWebhook = CreateWebhook'
  { _cwBranchFilter ::
      !(Maybe Text),
    _cwFilterGroups :: !(Maybe [[WebhookFilter]]),
    _cwBuildType :: !(Maybe WebhookBuildType),
    _cwProjectName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateWebhook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwBranchFilter' - A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
--
-- * 'cwFilterGroups' - An array of arrays of @WebhookFilter@ objects used to determine which webhooks are triggered. At least one @WebhookFilter@ in the array must specify @EVENT@ as its @type@ .  For a build to be triggered, at least one filter group in the @filterGroups@ array must pass. For a filter group to pass, each of its filters must pass.
--
-- * 'cwBuildType' - Specifies the type of build this webhook will trigger.
--
-- * 'cwProjectName' - The name of the AWS CodeBuild project.
createWebhook ::
  -- | 'cwProjectName'
  Text ->
  CreateWebhook
createWebhook pProjectName_ =
  CreateWebhook'
    { _cwBranchFilter = Nothing,
      _cwFilterGroups = Nothing,
      _cwBuildType = Nothing,
      _cwProjectName = pProjectName_
    }

-- | A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
cwBranchFilter :: Lens' CreateWebhook (Maybe Text)
cwBranchFilter = lens _cwBranchFilter (\s a -> s {_cwBranchFilter = a})

-- | An array of arrays of @WebhookFilter@ objects used to determine which webhooks are triggered. At least one @WebhookFilter@ in the array must specify @EVENT@ as its @type@ .  For a build to be triggered, at least one filter group in the @filterGroups@ array must pass. For a filter group to pass, each of its filters must pass.
cwFilterGroups :: Lens' CreateWebhook [[WebhookFilter]]
cwFilterGroups = lens _cwFilterGroups (\s a -> s {_cwFilterGroups = a}) . _Default . _Coerce

-- | Specifies the type of build this webhook will trigger.
cwBuildType :: Lens' CreateWebhook (Maybe WebhookBuildType)
cwBuildType = lens _cwBuildType (\s a -> s {_cwBuildType = a})

-- | The name of the AWS CodeBuild project.
cwProjectName :: Lens' CreateWebhook Text
cwProjectName = lens _cwProjectName (\s a -> s {_cwProjectName = a})

instance AWSRequest CreateWebhook where
  type Rs CreateWebhook = CreateWebhookResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          CreateWebhookResponse'
            <$> (x .?> "webhook") <*> (pure (fromEnum s))
      )

instance Hashable CreateWebhook

instance NFData CreateWebhook

instance ToHeaders CreateWebhook where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.CreateWebhook" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateWebhook where
  toJSON CreateWebhook' {..} =
    object
      ( catMaybes
          [ ("branchFilter" .=) <$> _cwBranchFilter,
            ("filterGroups" .=) <$> _cwFilterGroups,
            ("buildType" .=) <$> _cwBuildType,
            Just ("projectName" .= _cwProjectName)
          ]
      )

instance ToPath CreateWebhook where
  toPath = const "/"

instance ToQuery CreateWebhook where
  toQuery = const mempty

-- | /See:/ 'createWebhookResponse' smart constructor.
data CreateWebhookResponse = CreateWebhookResponse'
  { _cwrsWebhook ::
      !(Maybe Webhook),
    _cwrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateWebhookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cwrsWebhook' - Information about a webhook that connects repository events to a build project in AWS CodeBuild.
--
-- * 'cwrsResponseStatus' - -- | The response status code.
createWebhookResponse ::
  -- | 'cwrsResponseStatus'
  Int ->
  CreateWebhookResponse
createWebhookResponse pResponseStatus_ =
  CreateWebhookResponse'
    { _cwrsWebhook = Nothing,
      _cwrsResponseStatus = pResponseStatus_
    }

-- | Information about a webhook that connects repository events to a build project in AWS CodeBuild.
cwrsWebhook :: Lens' CreateWebhookResponse (Maybe Webhook)
cwrsWebhook = lens _cwrsWebhook (\s a -> s {_cwrsWebhook = a})

-- | -- | The response status code.
cwrsResponseStatus :: Lens' CreateWebhookResponse Int
cwrsResponseStatus = lens _cwrsResponseStatus (\s a -> s {_cwrsResponseStatus = a})

instance NFData CreateWebhookResponse
