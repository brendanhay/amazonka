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
-- Module      : Network.AWS.IoT.DeleteProvisioningTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a fleet provisioning template version.
module Network.AWS.IoT.DeleteProvisioningTemplateVersion
  ( -- * Creating a Request
    DeleteProvisioningTemplateVersion (..),
    newDeleteProvisioningTemplateVersion,

    -- * Request Lenses
    deleteProvisioningTemplateVersion_templateName,
    deleteProvisioningTemplateVersion_versionId,

    -- * Destructuring the Response
    DeleteProvisioningTemplateVersionResponse (..),
    newDeleteProvisioningTemplateVersionResponse,

    -- * Response Lenses
    deleteProvisioningTemplateVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteProvisioningTemplateVersion' smart constructor.
data DeleteProvisioningTemplateVersion = DeleteProvisioningTemplateVersion'
  { -- | The name of the fleet provisioning template version to delete.
    templateName :: Core.Text,
    -- | The fleet provisioning template version ID to delete.
    versionId :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProvisioningTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'deleteProvisioningTemplateVersion_templateName' - The name of the fleet provisioning template version to delete.
--
-- 'versionId', 'deleteProvisioningTemplateVersion_versionId' - The fleet provisioning template version ID to delete.
newDeleteProvisioningTemplateVersion ::
  -- | 'templateName'
  Core.Text ->
  -- | 'versionId'
  Core.Int ->
  DeleteProvisioningTemplateVersion
newDeleteProvisioningTemplateVersion
  pTemplateName_
  pVersionId_ =
    DeleteProvisioningTemplateVersion'
      { templateName =
          pTemplateName_,
        versionId = pVersionId_
      }

-- | The name of the fleet provisioning template version to delete.
deleteProvisioningTemplateVersion_templateName :: Lens.Lens' DeleteProvisioningTemplateVersion Core.Text
deleteProvisioningTemplateVersion_templateName = Lens.lens (\DeleteProvisioningTemplateVersion' {templateName} -> templateName) (\s@DeleteProvisioningTemplateVersion' {} a -> s {templateName = a} :: DeleteProvisioningTemplateVersion)

-- | The fleet provisioning template version ID to delete.
deleteProvisioningTemplateVersion_versionId :: Lens.Lens' DeleteProvisioningTemplateVersion Core.Int
deleteProvisioningTemplateVersion_versionId = Lens.lens (\DeleteProvisioningTemplateVersion' {versionId} -> versionId) (\s@DeleteProvisioningTemplateVersion' {} a -> s {versionId = a} :: DeleteProvisioningTemplateVersion)

instance
  Core.AWSRequest
    DeleteProvisioningTemplateVersion
  where
  type
    AWSResponse DeleteProvisioningTemplateVersion =
      DeleteProvisioningTemplateVersionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProvisioningTemplateVersionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteProvisioningTemplateVersion

instance
  Core.NFData
    DeleteProvisioningTemplateVersion

instance
  Core.ToHeaders
    DeleteProvisioningTemplateVersion
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DeleteProvisioningTemplateVersion
  where
  toPath DeleteProvisioningTemplateVersion' {..} =
    Core.mconcat
      [ "/provisioning-templates/",
        Core.toBS templateName,
        "/versions/",
        Core.toBS versionId
      ]

instance
  Core.ToQuery
    DeleteProvisioningTemplateVersion
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteProvisioningTemplateVersionResponse' smart constructor.
data DeleteProvisioningTemplateVersionResponse = DeleteProvisioningTemplateVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteProvisioningTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProvisioningTemplateVersionResponse_httpStatus' - The response's http status code.
newDeleteProvisioningTemplateVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteProvisioningTemplateVersionResponse
newDeleteProvisioningTemplateVersionResponse
  pHttpStatus_ =
    DeleteProvisioningTemplateVersionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteProvisioningTemplateVersionResponse_httpStatus :: Lens.Lens' DeleteProvisioningTemplateVersionResponse Core.Int
deleteProvisioningTemplateVersionResponse_httpStatus = Lens.lens (\DeleteProvisioningTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteProvisioningTemplateVersionResponse' {} a -> s {httpStatus = a} :: DeleteProvisioningTemplateVersionResponse)

instance
  Core.NFData
    DeleteProvisioningTemplateVersionResponse
