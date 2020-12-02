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
-- Module      : Network.AWS.CodeBuild.UpdateWebhook
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the webhook associated with an AWS CodeBuild build project.
module Network.AWS.CodeBuild.UpdateWebhook
  ( -- * Creating a Request
    updateWebhook,
    UpdateWebhook,

    -- * Request Lenses
    uwBranchFilter,
    uwRotateSecret,
    uwFilterGroups,
    uwBuildType,
    uwProjectName,

    -- * Destructuring the Response
    updateWebhookResponse,
    UpdateWebhookResponse,

    -- * Response Lenses
    uwrsWebhook,
    uwrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateWebhook' smart constructor.
data UpdateWebhook = UpdateWebhook'
  { _uwBranchFilter ::
      !(Maybe Text),
    _uwRotateSecret :: !(Maybe Bool),
    _uwFilterGroups :: !(Maybe [[WebhookFilter]]),
    _uwBuildType :: !(Maybe WebhookBuildType),
    _uwProjectName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateWebhook' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwBranchFilter' - A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
--
-- * 'uwRotateSecret' - A boolean value that specifies whether the associated GitHub repository's secret token should be updated. If you use Bitbucket for your repository, @rotateSecret@ is ignored.
--
-- * 'uwFilterGroups' - An array of arrays of @WebhookFilter@ objects used to determine if a webhook event can trigger a build. A filter group must contain at least one @EVENT@ @WebhookFilter@ .
--
-- * 'uwBuildType' - Specifies the type of build this webhook will trigger.
--
-- * 'uwProjectName' - The name of the AWS CodeBuild project.
updateWebhook ::
  -- | 'uwProjectName'
  Text ->
  UpdateWebhook
updateWebhook pProjectName_ =
  UpdateWebhook'
    { _uwBranchFilter = Nothing,
      _uwRotateSecret = Nothing,
      _uwFilterGroups = Nothing,
      _uwBuildType = Nothing,
      _uwProjectName = pProjectName_
    }

-- | A regular expression used to determine which repository branches are built when a webhook is triggered. If the name of a branch matches the regular expression, then it is built. If @branchFilter@ is empty, then all branches are built.
uwBranchFilter :: Lens' UpdateWebhook (Maybe Text)
uwBranchFilter = lens _uwBranchFilter (\s a -> s {_uwBranchFilter = a})

-- | A boolean value that specifies whether the associated GitHub repository's secret token should be updated. If you use Bitbucket for your repository, @rotateSecret@ is ignored.
uwRotateSecret :: Lens' UpdateWebhook (Maybe Bool)
uwRotateSecret = lens _uwRotateSecret (\s a -> s {_uwRotateSecret = a})

-- | An array of arrays of @WebhookFilter@ objects used to determine if a webhook event can trigger a build. A filter group must contain at least one @EVENT@ @WebhookFilter@ .
uwFilterGroups :: Lens' UpdateWebhook [[WebhookFilter]]
uwFilterGroups = lens _uwFilterGroups (\s a -> s {_uwFilterGroups = a}) . _Default . _Coerce

-- | Specifies the type of build this webhook will trigger.
uwBuildType :: Lens' UpdateWebhook (Maybe WebhookBuildType)
uwBuildType = lens _uwBuildType (\s a -> s {_uwBuildType = a})

-- | The name of the AWS CodeBuild project.
uwProjectName :: Lens' UpdateWebhook Text
uwProjectName = lens _uwProjectName (\s a -> s {_uwProjectName = a})

instance AWSRequest UpdateWebhook where
  type Rs UpdateWebhook = UpdateWebhookResponse
  request = postJSON codeBuild
  response =
    receiveJSON
      ( \s h x ->
          UpdateWebhookResponse'
            <$> (x .?> "webhook") <*> (pure (fromEnum s))
      )

instance Hashable UpdateWebhook

instance NFData UpdateWebhook

instance ToHeaders UpdateWebhook where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CodeBuild_20161006.UpdateWebhook" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateWebhook where
  toJSON UpdateWebhook' {..} =
    object
      ( catMaybes
          [ ("branchFilter" .=) <$> _uwBranchFilter,
            ("rotateSecret" .=) <$> _uwRotateSecret,
            ("filterGroups" .=) <$> _uwFilterGroups,
            ("buildType" .=) <$> _uwBuildType,
            Just ("projectName" .= _uwProjectName)
          ]
      )

instance ToPath UpdateWebhook where
  toPath = const "/"

instance ToQuery UpdateWebhook where
  toQuery = const mempty

-- | /See:/ 'updateWebhookResponse' smart constructor.
data UpdateWebhookResponse = UpdateWebhookResponse'
  { _uwrsWebhook ::
      !(Maybe Webhook),
    _uwrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateWebhookResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uwrsWebhook' - Information about a repository's webhook that is associated with a project in AWS CodeBuild.
--
-- * 'uwrsResponseStatus' - -- | The response status code.
updateWebhookResponse ::
  -- | 'uwrsResponseStatus'
  Int ->
  UpdateWebhookResponse
updateWebhookResponse pResponseStatus_ =
  UpdateWebhookResponse'
    { _uwrsWebhook = Nothing,
      _uwrsResponseStatus = pResponseStatus_
    }

-- | Information about a repository's webhook that is associated with a project in AWS CodeBuild.
uwrsWebhook :: Lens' UpdateWebhookResponse (Maybe Webhook)
uwrsWebhook = lens _uwrsWebhook (\s a -> s {_uwrsWebhook = a})

-- | -- | The response status code.
uwrsResponseStatus :: Lens' UpdateWebhookResponse Int
uwrsResponseStatus = lens _uwrsResponseStatus (\s a -> s {_uwrsResponseStatus = a})

instance NFData UpdateWebhookResponse
