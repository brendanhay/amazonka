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
-- Module      : Amazonka.IoT.DeleteProvisioningTemplateVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a provisioning template version.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DeleteProvisioningTemplateVersion>
-- action.
module Amazonka.IoT.DeleteProvisioningTemplateVersion
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProvisioningTemplateVersion' smart constructor.
data DeleteProvisioningTemplateVersion = DeleteProvisioningTemplateVersion'
  { -- | The name of the provisioning template version to delete.
    templateName :: Prelude.Text,
    -- | The provisioning template version ID to delete.
    versionId :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProvisioningTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'deleteProvisioningTemplateVersion_templateName' - The name of the provisioning template version to delete.
--
-- 'versionId', 'deleteProvisioningTemplateVersion_versionId' - The provisioning template version ID to delete.
newDeleteProvisioningTemplateVersion ::
  -- | 'templateName'
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Int ->
  DeleteProvisioningTemplateVersion
newDeleteProvisioningTemplateVersion
  pTemplateName_
  pVersionId_ =
    DeleteProvisioningTemplateVersion'
      { templateName =
          pTemplateName_,
        versionId = pVersionId_
      }

-- | The name of the provisioning template version to delete.
deleteProvisioningTemplateVersion_templateName :: Lens.Lens' DeleteProvisioningTemplateVersion Prelude.Text
deleteProvisioningTemplateVersion_templateName = Lens.lens (\DeleteProvisioningTemplateVersion' {templateName} -> templateName) (\s@DeleteProvisioningTemplateVersion' {} a -> s {templateName = a} :: DeleteProvisioningTemplateVersion)

-- | The provisioning template version ID to delete.
deleteProvisioningTemplateVersion_versionId :: Lens.Lens' DeleteProvisioningTemplateVersion Prelude.Int
deleteProvisioningTemplateVersion_versionId = Lens.lens (\DeleteProvisioningTemplateVersion' {versionId} -> versionId) (\s@DeleteProvisioningTemplateVersion' {} a -> s {versionId = a} :: DeleteProvisioningTemplateVersion)

instance
  Core.AWSRequest
    DeleteProvisioningTemplateVersion
  where
  type
    AWSResponse DeleteProvisioningTemplateVersion =
      DeleteProvisioningTemplateVersionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteProvisioningTemplateVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteProvisioningTemplateVersion
  where
  hashWithSalt
    _salt
    DeleteProvisioningTemplateVersion' {..} =
      _salt
        `Prelude.hashWithSalt` templateName
        `Prelude.hashWithSalt` versionId

instance
  Prelude.NFData
    DeleteProvisioningTemplateVersion
  where
  rnf DeleteProvisioningTemplateVersion' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf versionId

instance
  Data.ToHeaders
    DeleteProvisioningTemplateVersion
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    DeleteProvisioningTemplateVersion
  where
  toPath DeleteProvisioningTemplateVersion' {..} =
    Prelude.mconcat
      [ "/provisioning-templates/",
        Data.toBS templateName,
        "/versions/",
        Data.toBS versionId
      ]

instance
  Data.ToQuery
    DeleteProvisioningTemplateVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProvisioningTemplateVersionResponse' smart constructor.
data DeleteProvisioningTemplateVersionResponse = DeleteProvisioningTemplateVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteProvisioningTemplateVersionResponse
newDeleteProvisioningTemplateVersionResponse
  pHttpStatus_ =
    DeleteProvisioningTemplateVersionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteProvisioningTemplateVersionResponse_httpStatus :: Lens.Lens' DeleteProvisioningTemplateVersionResponse Prelude.Int
deleteProvisioningTemplateVersionResponse_httpStatus = Lens.lens (\DeleteProvisioningTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteProvisioningTemplateVersionResponse' {} a -> s {httpStatus = a} :: DeleteProvisioningTemplateVersionResponse)

instance
  Prelude.NFData
    DeleteProvisioningTemplateVersionResponse
  where
  rnf DeleteProvisioningTemplateVersionResponse' {..} =
    Prelude.rnf httpStatus
