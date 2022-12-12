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
-- Module      : Amazonka.CodeBuild.UpdateWebhook
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the webhook associated with an CodeBuild build project.
--
-- If you use Bitbucket for your repository, @rotateSecret@ is ignored.
module Amazonka.CodeBuild.UpdateWebhook
  ( -- * Creating a Request
    UpdateWebhook (..),
    newUpdateWebhook,

    -- * Request Lenses
    updateWebhook_branchFilter,
    updateWebhook_buildType,
    updateWebhook_filterGroups,
    updateWebhook_rotateSecret,
    updateWebhook_projectName,

    -- * Destructuring the Response
    UpdateWebhookResponse (..),
    newUpdateWebhookResponse,

    -- * Response Lenses
    updateWebhookResponse_webhook,
    updateWebhookResponse_httpStatus,
  )
where

import Amazonka.CodeBuild.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWebhook' smart constructor.
data UpdateWebhook = UpdateWebhook'
  { -- | A regular expression used to determine which repository branches are
    -- built when a webhook is triggered. If the name of a branch matches the
    -- regular expression, then it is built. If @branchFilter@ is empty, then
    -- all branches are built.
    --
    -- It is recommended that you use @filterGroups@ instead of @branchFilter@.
    branchFilter :: Prelude.Maybe Prelude.Text,
    -- | Specifies the type of build this webhook will trigger.
    buildType :: Prelude.Maybe WebhookBuildType,
    -- | An array of arrays of @WebhookFilter@ objects used to determine if a
    -- webhook event can trigger a build. A filter group must contain at least
    -- one @EVENT@ @WebhookFilter@.
    filterGroups :: Prelude.Maybe [[WebhookFilter]],
    -- | A boolean value that specifies whether the associated GitHub
    -- repository\'s secret token should be updated. If you use Bitbucket for
    -- your repository, @rotateSecret@ is ignored.
    rotateSecret :: Prelude.Maybe Prelude.Bool,
    -- | The name of the CodeBuild project.
    projectName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWebhook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'branchFilter', 'updateWebhook_branchFilter' - A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
--
-- 'buildType', 'updateWebhook_buildType' - Specifies the type of build this webhook will trigger.
--
-- 'filterGroups', 'updateWebhook_filterGroups' - An array of arrays of @WebhookFilter@ objects used to determine if a
-- webhook event can trigger a build. A filter group must contain at least
-- one @EVENT@ @WebhookFilter@.
--
-- 'rotateSecret', 'updateWebhook_rotateSecret' - A boolean value that specifies whether the associated GitHub
-- repository\'s secret token should be updated. If you use Bitbucket for
-- your repository, @rotateSecret@ is ignored.
--
-- 'projectName', 'updateWebhook_projectName' - The name of the CodeBuild project.
newUpdateWebhook ::
  -- | 'projectName'
  Prelude.Text ->
  UpdateWebhook
newUpdateWebhook pProjectName_ =
  UpdateWebhook'
    { branchFilter = Prelude.Nothing,
      buildType = Prelude.Nothing,
      filterGroups = Prelude.Nothing,
      rotateSecret = Prelude.Nothing,
      projectName = pProjectName_
    }

-- | A regular expression used to determine which repository branches are
-- built when a webhook is triggered. If the name of a branch matches the
-- regular expression, then it is built. If @branchFilter@ is empty, then
-- all branches are built.
--
-- It is recommended that you use @filterGroups@ instead of @branchFilter@.
updateWebhook_branchFilter :: Lens.Lens' UpdateWebhook (Prelude.Maybe Prelude.Text)
updateWebhook_branchFilter = Lens.lens (\UpdateWebhook' {branchFilter} -> branchFilter) (\s@UpdateWebhook' {} a -> s {branchFilter = a} :: UpdateWebhook)

-- | Specifies the type of build this webhook will trigger.
updateWebhook_buildType :: Lens.Lens' UpdateWebhook (Prelude.Maybe WebhookBuildType)
updateWebhook_buildType = Lens.lens (\UpdateWebhook' {buildType} -> buildType) (\s@UpdateWebhook' {} a -> s {buildType = a} :: UpdateWebhook)

-- | An array of arrays of @WebhookFilter@ objects used to determine if a
-- webhook event can trigger a build. A filter group must contain at least
-- one @EVENT@ @WebhookFilter@.
updateWebhook_filterGroups :: Lens.Lens' UpdateWebhook (Prelude.Maybe [[WebhookFilter]])
updateWebhook_filterGroups = Lens.lens (\UpdateWebhook' {filterGroups} -> filterGroups) (\s@UpdateWebhook' {} a -> s {filterGroups = a} :: UpdateWebhook) Prelude.. Lens.mapping Lens.coerced

-- | A boolean value that specifies whether the associated GitHub
-- repository\'s secret token should be updated. If you use Bitbucket for
-- your repository, @rotateSecret@ is ignored.
updateWebhook_rotateSecret :: Lens.Lens' UpdateWebhook (Prelude.Maybe Prelude.Bool)
updateWebhook_rotateSecret = Lens.lens (\UpdateWebhook' {rotateSecret} -> rotateSecret) (\s@UpdateWebhook' {} a -> s {rotateSecret = a} :: UpdateWebhook)

-- | The name of the CodeBuild project.
updateWebhook_projectName :: Lens.Lens' UpdateWebhook Prelude.Text
updateWebhook_projectName = Lens.lens (\UpdateWebhook' {projectName} -> projectName) (\s@UpdateWebhook' {} a -> s {projectName = a} :: UpdateWebhook)

instance Core.AWSRequest UpdateWebhook where
  type
    AWSResponse UpdateWebhook =
      UpdateWebhookResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWebhookResponse'
            Prelude.<$> (x Data..?> "webhook")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWebhook where
  hashWithSalt _salt UpdateWebhook' {..} =
    _salt `Prelude.hashWithSalt` branchFilter
      `Prelude.hashWithSalt` buildType
      `Prelude.hashWithSalt` filterGroups
      `Prelude.hashWithSalt` rotateSecret
      `Prelude.hashWithSalt` projectName

instance Prelude.NFData UpdateWebhook where
  rnf UpdateWebhook' {..} =
    Prelude.rnf branchFilter
      `Prelude.seq` Prelude.rnf buildType
      `Prelude.seq` Prelude.rnf filterGroups
      `Prelude.seq` Prelude.rnf rotateSecret
      `Prelude.seq` Prelude.rnf projectName

instance Data.ToHeaders UpdateWebhook where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeBuild_20161006.UpdateWebhook" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWebhook where
  toJSON UpdateWebhook' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("branchFilter" Data..=) Prelude.<$> branchFilter,
            ("buildType" Data..=) Prelude.<$> buildType,
            ("filterGroups" Data..=) Prelude.<$> filterGroups,
            ("rotateSecret" Data..=) Prelude.<$> rotateSecret,
            Prelude.Just ("projectName" Data..= projectName)
          ]
      )

instance Data.ToPath UpdateWebhook where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateWebhook where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWebhookResponse' smart constructor.
data UpdateWebhookResponse = UpdateWebhookResponse'
  { -- | Information about a repository\'s webhook that is associated with a
    -- project in CodeBuild.
    webhook :: Prelude.Maybe Webhook,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWebhookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'webhook', 'updateWebhookResponse_webhook' - Information about a repository\'s webhook that is associated with a
-- project in CodeBuild.
--
-- 'httpStatus', 'updateWebhookResponse_httpStatus' - The response's http status code.
newUpdateWebhookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWebhookResponse
newUpdateWebhookResponse pHttpStatus_ =
  UpdateWebhookResponse'
    { webhook = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a repository\'s webhook that is associated with a
-- project in CodeBuild.
updateWebhookResponse_webhook :: Lens.Lens' UpdateWebhookResponse (Prelude.Maybe Webhook)
updateWebhookResponse_webhook = Lens.lens (\UpdateWebhookResponse' {webhook} -> webhook) (\s@UpdateWebhookResponse' {} a -> s {webhook = a} :: UpdateWebhookResponse)

-- | The response's http status code.
updateWebhookResponse_httpStatus :: Lens.Lens' UpdateWebhookResponse Prelude.Int
updateWebhookResponse_httpStatus = Lens.lens (\UpdateWebhookResponse' {httpStatus} -> httpStatus) (\s@UpdateWebhookResponse' {} a -> s {httpStatus = a} :: UpdateWebhookResponse)

instance Prelude.NFData UpdateWebhookResponse where
  rnf UpdateWebhookResponse' {..} =
    Prelude.rnf webhook
      `Prelude.seq` Prelude.rnf httpStatus
