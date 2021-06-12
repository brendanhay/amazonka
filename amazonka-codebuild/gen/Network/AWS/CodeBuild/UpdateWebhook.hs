{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.UpdateWebhook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the webhook associated with an AWS CodeBuild build project.
--
-- If you use Bitbucket for your repository, @rotateSecret@ is ignored.
module Network.AWS.CodeBuild.UpdateWebhook
  ( -- * Creating a Request
    UpdateWebhook (..),
    newUpdateWebhook,

    -- * Request Lenses
    updateWebhook_rotateSecret,
    updateWebhook_branchFilter,
    updateWebhook_filterGroups,
    updateWebhook_buildType,
    updateWebhook_projectName,

    -- * Destructuring the Response
    UpdateWebhookResponse (..),
    newUpdateWebhookResponse,

    -- * Response Lenses
    updateWebhookResponse_webhook,
    updateWebhookResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateWebhook' smart constructor.
data UpdateWebhook = UpdateWebhook'
  { -- | A boolean value that specifies whether the associated GitHub
    -- repository\'s secret token should be updated. If you use Bitbucket for
    -- your repository, @rotateSecret@ is ignored.
    rotateSecret :: Core.Maybe Core.Bool,
    -- | A regular expression used to determine which repository branches are
    -- built when a webhook is triggered. If the name of a branch matches the
    -- regular expression, then it is built. If @branchFilter@ is empty, then
    -- all branches are built.
    --
    -- It is recommended that you use @filterGroups@ instead of @branchFilter@.
    branchFilter :: Core.Maybe Core.Text,
    -- | An array of arrays of @WebhookFilter@ objects used to determine if a
    -- webhook event can trigger a build. A filter group must contain at least
    -- one @EVENT@ @WebhookFilter@.
    filterGroups :: Core.Maybe [[WebhookFilter]],
    -- | Specifies the type of build this webhook will trigger.
    buildType :: Core.Maybe WebhookBuildType,
    -- | The name of the AWS CodeBuild project.
    projectName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateWebhook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rotateSecret', 'updateWebhook_rotateSecret' - A boolean value that specifies whether the associated GitHub
-- repository\'s secret token should be updated. If you use Bitbucket for
-- your repository, @rotateSecret@ is ignored.
--
-- 'branchFilter', 'updateWebhook_branchFilter' - A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
--
-- 'filterGroups', 'updateWebhook_filterGroups' - An array of arrays of @WebhookFilter@ objects used to determine if a
-- webhook event can trigger a build. A filter group must contain at least
-- one @EVENT@ @WebhookFilter@.
--
-- 'buildType', 'updateWebhook_buildType' - Specifies the type of build this webhook will trigger.
--
-- 'projectName', 'updateWebhook_projectName' - The name of the AWS CodeBuild project.
newUpdateWebhook ::
  -- | 'projectName'
  Core.Text ->
  UpdateWebhook
newUpdateWebhook pProjectName_ =
  UpdateWebhook'
    { rotateSecret = Core.Nothing,
      branchFilter = Core.Nothing,
      filterGroups = Core.Nothing,
      buildType = Core.Nothing,
      projectName = pProjectName_
    }

-- | A boolean value that specifies whether the associated GitHub
-- repository\'s secret token should be updated. If you use Bitbucket for
-- your repository, @rotateSecret@ is ignored.
updateWebhook_rotateSecret :: Lens.Lens' UpdateWebhook (Core.Maybe Core.Bool)
updateWebhook_rotateSecret = Lens.lens (\UpdateWebhook' {rotateSecret} -> rotateSecret) (\s@UpdateWebhook' {} a -> s {rotateSecret = a} :: UpdateWebhook)

-- | A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
updateWebhook_branchFilter :: Lens.Lens' UpdateWebhook (Core.Maybe Core.Text)
updateWebhook_branchFilter = Lens.lens (\UpdateWebhook' {branchFilter} -> branchFilter) (\s@UpdateWebhook' {} a -> s {branchFilter = a} :: UpdateWebhook)

-- | An array of arrays of @WebhookFilter@ objects used to determine if a
-- webhook event can trigger a build. A filter group must contain at least
-- one @EVENT@ @WebhookFilter@.
updateWebhook_filterGroups :: Lens.Lens' UpdateWebhook (Core.Maybe [[WebhookFilter]])
updateWebhook_filterGroups = Lens.lens (\UpdateWebhook' {filterGroups} -> filterGroups) (\s@UpdateWebhook' {} a -> s {filterGroups = a} :: UpdateWebhook) Core.. Lens.mapping Lens._Coerce

-- | Specifies the type of build this webhook will trigger.
updateWebhook_buildType :: Lens.Lens' UpdateWebhook (Core.Maybe WebhookBuildType)
updateWebhook_buildType = Lens.lens (\UpdateWebhook' {buildType} -> buildType) (\s@UpdateWebhook' {} a -> s {buildType = a} :: UpdateWebhook)

-- | The name of the AWS CodeBuild project.
updateWebhook_projectName :: Lens.Lens' UpdateWebhook Core.Text
updateWebhook_projectName = Lens.lens (\UpdateWebhook' {projectName} -> projectName) (\s@UpdateWebhook' {} a -> s {projectName = a} :: UpdateWebhook)

instance Core.AWSRequest UpdateWebhook where
  type
    AWSResponse UpdateWebhook =
      UpdateWebhookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWebhookResponse'
            Core.<$> (x Core..?> "webhook")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateWebhook

instance Core.NFData UpdateWebhook

instance Core.ToHeaders UpdateWebhook where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.UpdateWebhook" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateWebhook where
  toJSON UpdateWebhook' {..} =
    Core.object
      ( Core.catMaybes
          [ ("rotateSecret" Core..=) Core.<$> rotateSecret,
            ("branchFilter" Core..=) Core.<$> branchFilter,
            ("filterGroups" Core..=) Core.<$> filterGroups,
            ("buildType" Core..=) Core.<$> buildType,
            Core.Just ("projectName" Core..= projectName)
          ]
      )

instance Core.ToPath UpdateWebhook where
  toPath = Core.const "/"

instance Core.ToQuery UpdateWebhook where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateWebhookResponse' smart constructor.
data UpdateWebhookResponse = UpdateWebhookResponse'
  { -- | Information about a repository\'s webhook that is associated with a
    -- project in AWS CodeBuild.
    webhook :: Core.Maybe Webhook,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateWebhookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webhook', 'updateWebhookResponse_webhook' - Information about a repository\'s webhook that is associated with a
-- project in AWS CodeBuild.
--
-- 'httpStatus', 'updateWebhookResponse_httpStatus' - The response's http status code.
newUpdateWebhookResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateWebhookResponse
newUpdateWebhookResponse pHttpStatus_ =
  UpdateWebhookResponse'
    { webhook = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a repository\'s webhook that is associated with a
-- project in AWS CodeBuild.
updateWebhookResponse_webhook :: Lens.Lens' UpdateWebhookResponse (Core.Maybe Webhook)
updateWebhookResponse_webhook = Lens.lens (\UpdateWebhookResponse' {webhook} -> webhook) (\s@UpdateWebhookResponse' {} a -> s {webhook = a} :: UpdateWebhookResponse)

-- | The response's http status code.
updateWebhookResponse_httpStatus :: Lens.Lens' UpdateWebhookResponse Core.Int
updateWebhookResponse_httpStatus = Lens.lens (\UpdateWebhookResponse' {httpStatus} -> httpStatus) (\s@UpdateWebhookResponse' {} a -> s {httpStatus = a} :: UpdateWebhookResponse)

instance Core.NFData UpdateWebhookResponse
