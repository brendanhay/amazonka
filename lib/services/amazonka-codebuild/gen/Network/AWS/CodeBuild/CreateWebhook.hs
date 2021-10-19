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
-- Module      : Network.AWS.CodeBuild.CreateWebhook
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For an existing CodeBuild build project that has its source code stored
-- in a GitHub or Bitbucket repository, enables CodeBuild to start
-- rebuilding the source code every time a code change is pushed to the
-- repository.
--
-- If you enable webhooks for an CodeBuild project, and the project is used
-- as a build step in CodePipeline, then two identical builds are created
-- for each commit. One build is triggered through webhooks, and one
-- through CodePipeline. Because billing is on a per-build basis, you are
-- billed for both builds. Therefore, if you are using CodePipeline, we
-- recommend that you disable webhooks in CodeBuild. In the CodeBuild
-- console, clear the Webhook box. For more information, see step 5 in
-- <https://docs.aws.amazon.com/codebuild/latest/userguide/change-project.html#change-project-console Change a Build Project\'s Settings>.
module Network.AWS.CodeBuild.CreateWebhook
  ( -- * Creating a Request
    CreateWebhook (..),
    newCreateWebhook,

    -- * Request Lenses
    createWebhook_branchFilter,
    createWebhook_filterGroups,
    createWebhook_buildType,
    createWebhook_projectName,

    -- * Destructuring the Response
    CreateWebhookResponse (..),
    newCreateWebhookResponse,

    -- * Response Lenses
    createWebhookResponse_webhook,
    createWebhookResponse_httpStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateWebhook' smart constructor.
data CreateWebhook = CreateWebhook'
  { -- | A regular expression used to determine which repository branches are
    -- built when a webhook is triggered. If the name of a branch matches the
    -- regular expression, then it is built. If @branchFilter@ is empty, then
    -- all branches are built.
    --
    -- It is recommended that you use @filterGroups@ instead of @branchFilter@.
    branchFilter :: Prelude.Maybe Prelude.Text,
    -- | An array of arrays of @WebhookFilter@ objects used to determine which
    -- webhooks are triggered. At least one @WebhookFilter@ in the array must
    -- specify @EVENT@ as its @type@.
    --
    -- For a build to be triggered, at least one filter group in the
    -- @filterGroups@ array must pass. For a filter group to pass, each of its
    -- filters must pass.
    filterGroups :: Prelude.Maybe [[WebhookFilter]],
    -- | Specifies the type of build this webhook will trigger.
    buildType :: Prelude.Maybe WebhookBuildType,
    -- | The name of the CodeBuild project.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWebhook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchFilter', 'createWebhook_branchFilter' - A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
--
-- 'filterGroups', 'createWebhook_filterGroups' - An array of arrays of @WebhookFilter@ objects used to determine which
-- webhooks are triggered. At least one @WebhookFilter@ in the array must
-- specify @EVENT@ as its @type@.
--
-- For a build to be triggered, at least one filter group in the
-- @filterGroups@ array must pass. For a filter group to pass, each of its
-- filters must pass.
--
-- 'buildType', 'createWebhook_buildType' - Specifies the type of build this webhook will trigger.
--
-- 'projectName', 'createWebhook_projectName' - The name of the CodeBuild project.
newCreateWebhook ::
  -- | 'projectName'
  Prelude.Text ->
  CreateWebhook
newCreateWebhook pProjectName_ =
  CreateWebhook'
    { branchFilter = Prelude.Nothing,
      filterGroups = Prelude.Nothing,
      buildType = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
createWebhook_branchFilter :: Lens.Lens' CreateWebhook (Prelude.Maybe Prelude.Text)
createWebhook_branchFilter = Lens.lens (\CreateWebhook' {branchFilter} -> branchFilter) (\s@CreateWebhook' {} a -> s {branchFilter = a} :: CreateWebhook)

-- | An array of arrays of @WebhookFilter@ objects used to determine which
-- webhooks are triggered. At least one @WebhookFilter@ in the array must
-- specify @EVENT@ as its @type@.
--
-- For a build to be triggered, at least one filter group in the
-- @filterGroups@ array must pass. For a filter group to pass, each of its
-- filters must pass.
createWebhook_filterGroups :: Lens.Lens' CreateWebhook (Prelude.Maybe [[WebhookFilter]])
createWebhook_filterGroups = Lens.lens (\CreateWebhook' {filterGroups} -> filterGroups) (\s@CreateWebhook' {} a -> s {filterGroups = a} :: CreateWebhook) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the type of build this webhook will trigger.
createWebhook_buildType :: Lens.Lens' CreateWebhook (Prelude.Maybe WebhookBuildType)
createWebhook_buildType = Lens.lens (\CreateWebhook' {buildType} -> buildType) (\s@CreateWebhook' {} a -> s {buildType = a} :: CreateWebhook)

-- | The name of the CodeBuild project.
createWebhook_projectName :: Lens.Lens' CreateWebhook Prelude.Text
createWebhook_projectName = Lens.lens (\CreateWebhook' {projectName} -> projectName) (\s@CreateWebhook' {} a -> s {projectName = a} :: CreateWebhook)

instance Core.AWSRequest CreateWebhook where
  type
    AWSResponse CreateWebhook =
      CreateWebhookResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWebhookResponse'
            Prelude.<$> (x Core..?> "webhook")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWebhook

instance Prelude.NFData CreateWebhook

instance Core.ToHeaders CreateWebhook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeBuild_20161006.CreateWebhook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWebhook where
  toJSON CreateWebhook' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("branchFilter" Core..=) Prelude.<$> branchFilter,
            ("filterGroups" Core..=) Prelude.<$> filterGroups,
            ("buildType" Core..=) Prelude.<$> buildType,
            Prelude.Just ("projectName" Core..= projectName)
          ]
      )

instance Core.ToPath CreateWebhook where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateWebhook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWebhookResponse' smart constructor.
data CreateWebhookResponse = CreateWebhookResponse'
  { -- | Information about a webhook that connects repository events to a build
    -- project in CodeBuild.
    webhook :: Prelude.Maybe Webhook,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWebhookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webhook', 'createWebhookResponse_webhook' - Information about a webhook that connects repository events to a build
-- project in CodeBuild.
--
-- 'httpStatus', 'createWebhookResponse_httpStatus' - The response's http status code.
newCreateWebhookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWebhookResponse
newCreateWebhookResponse pHttpStatus_ =
  CreateWebhookResponse'
    { webhook = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a webhook that connects repository events to a build
-- project in CodeBuild.
createWebhookResponse_webhook :: Lens.Lens' CreateWebhookResponse (Prelude.Maybe Webhook)
createWebhookResponse_webhook = Lens.lens (\CreateWebhookResponse' {webhook} -> webhook) (\s@CreateWebhookResponse' {} a -> s {webhook = a} :: CreateWebhookResponse)

-- | The response's http status code.
createWebhookResponse_httpStatus :: Lens.Lens' CreateWebhookResponse Prelude.Int
createWebhookResponse_httpStatus = Lens.lens (\CreateWebhookResponse' {httpStatus} -> httpStatus) (\s@CreateWebhookResponse' {} a -> s {httpStatus = a} :: CreateWebhookResponse)

instance Prelude.NFData CreateWebhookResponse
