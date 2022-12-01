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
-- Module      : Amazonka.Proton.DeleteServiceTemplateVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If no other minor versions of a service template exist, delete a major
-- version of the service template if it\'s not the @Recommended@ version.
-- Delete the @Recommended@ version of the service template if no other
-- major versions or minor versions of the service template exist. A major
-- version of a service template is a version that /isn\'t/ backwards
-- compatible.
--
-- Delete a minor version of a service template if it\'s not the
-- @Recommended@ version. Delete a @Recommended@ minor version of the
-- service template if no other minor versions of the service template
-- exist. A minor version of a service template is a version that\'s
-- backwards compatible.
module Amazonka.Proton.DeleteServiceTemplateVersion
  ( -- * Creating a Request
    DeleteServiceTemplateVersion (..),
    newDeleteServiceTemplateVersion,

    -- * Request Lenses
    deleteServiceTemplateVersion_majorVersion,
    deleteServiceTemplateVersion_minorVersion,
    deleteServiceTemplateVersion_templateName,

    -- * Destructuring the Response
    DeleteServiceTemplateVersionResponse (..),
    newDeleteServiceTemplateVersionResponse,

    -- * Response Lenses
    deleteServiceTemplateVersionResponse_serviceTemplateVersion,
    deleteServiceTemplateVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteServiceTemplateVersion' smart constructor.
data DeleteServiceTemplateVersion = DeleteServiceTemplateVersion'
  { -- | The service template major version to delete.
    majorVersion :: Prelude.Text,
    -- | The service template minor version to delete.
    minorVersion :: Prelude.Text,
    -- | The name of the service template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'majorVersion', 'deleteServiceTemplateVersion_majorVersion' - The service template major version to delete.
--
-- 'minorVersion', 'deleteServiceTemplateVersion_minorVersion' - The service template minor version to delete.
--
-- 'templateName', 'deleteServiceTemplateVersion_templateName' - The name of the service template.
newDeleteServiceTemplateVersion ::
  -- | 'majorVersion'
  Prelude.Text ->
  -- | 'minorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  DeleteServiceTemplateVersion
newDeleteServiceTemplateVersion
  pMajorVersion_
  pMinorVersion_
  pTemplateName_ =
    DeleteServiceTemplateVersion'
      { majorVersion =
          pMajorVersion_,
        minorVersion = pMinorVersion_,
        templateName = pTemplateName_
      }

-- | The service template major version to delete.
deleteServiceTemplateVersion_majorVersion :: Lens.Lens' DeleteServiceTemplateVersion Prelude.Text
deleteServiceTemplateVersion_majorVersion = Lens.lens (\DeleteServiceTemplateVersion' {majorVersion} -> majorVersion) (\s@DeleteServiceTemplateVersion' {} a -> s {majorVersion = a} :: DeleteServiceTemplateVersion)

-- | The service template minor version to delete.
deleteServiceTemplateVersion_minorVersion :: Lens.Lens' DeleteServiceTemplateVersion Prelude.Text
deleteServiceTemplateVersion_minorVersion = Lens.lens (\DeleteServiceTemplateVersion' {minorVersion} -> minorVersion) (\s@DeleteServiceTemplateVersion' {} a -> s {minorVersion = a} :: DeleteServiceTemplateVersion)

-- | The name of the service template.
deleteServiceTemplateVersion_templateName :: Lens.Lens' DeleteServiceTemplateVersion Prelude.Text
deleteServiceTemplateVersion_templateName = Lens.lens (\DeleteServiceTemplateVersion' {templateName} -> templateName) (\s@DeleteServiceTemplateVersion' {} a -> s {templateName = a} :: DeleteServiceTemplateVersion)

instance Core.AWSRequest DeleteServiceTemplateVersion where
  type
    AWSResponse DeleteServiceTemplateVersion =
      DeleteServiceTemplateVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteServiceTemplateVersionResponse'
            Prelude.<$> (x Core..?> "serviceTemplateVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteServiceTemplateVersion
  where
  hashWithSalt _salt DeleteServiceTemplateVersion' {..} =
    _salt `Prelude.hashWithSalt` majorVersion
      `Prelude.hashWithSalt` minorVersion
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData DeleteServiceTemplateVersion where
  rnf DeleteServiceTemplateVersion' {..} =
    Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf templateName

instance Core.ToHeaders DeleteServiceTemplateVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.DeleteServiceTemplateVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteServiceTemplateVersion where
  toJSON DeleteServiceTemplateVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("majorVersion" Core..= majorVersion),
            Prelude.Just ("minorVersion" Core..= minorVersion),
            Prelude.Just ("templateName" Core..= templateName)
          ]
      )

instance Core.ToPath DeleteServiceTemplateVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteServiceTemplateVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteServiceTemplateVersionResponse' smart constructor.
data DeleteServiceTemplateVersionResponse = DeleteServiceTemplateVersionResponse'
  { -- | The detailed data of the service template version being deleted.
    serviceTemplateVersion :: Prelude.Maybe ServiceTemplateVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteServiceTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceTemplateVersion', 'deleteServiceTemplateVersionResponse_serviceTemplateVersion' - The detailed data of the service template version being deleted.
--
-- 'httpStatus', 'deleteServiceTemplateVersionResponse_httpStatus' - The response's http status code.
newDeleteServiceTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteServiceTemplateVersionResponse
newDeleteServiceTemplateVersionResponse pHttpStatus_ =
  DeleteServiceTemplateVersionResponse'
    { serviceTemplateVersion =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The detailed data of the service template version being deleted.
deleteServiceTemplateVersionResponse_serviceTemplateVersion :: Lens.Lens' DeleteServiceTemplateVersionResponse (Prelude.Maybe ServiceTemplateVersion)
deleteServiceTemplateVersionResponse_serviceTemplateVersion = Lens.lens (\DeleteServiceTemplateVersionResponse' {serviceTemplateVersion} -> serviceTemplateVersion) (\s@DeleteServiceTemplateVersionResponse' {} a -> s {serviceTemplateVersion = a} :: DeleteServiceTemplateVersionResponse)

-- | The response's http status code.
deleteServiceTemplateVersionResponse_httpStatus :: Lens.Lens' DeleteServiceTemplateVersionResponse Prelude.Int
deleteServiceTemplateVersionResponse_httpStatus = Lens.lens (\DeleteServiceTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteServiceTemplateVersionResponse' {} a -> s {httpStatus = a} :: DeleteServiceTemplateVersionResponse)

instance
  Prelude.NFData
    DeleteServiceTemplateVersionResponse
  where
  rnf DeleteServiceTemplateVersionResponse' {..} =
    Prelude.rnf serviceTemplateVersion
      `Prelude.seq` Prelude.rnf httpStatus
