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
-- Module      : Amazonka.Proton.DeleteEnvironmentTemplateVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If no other minor versions of an environment template exist, delete a
-- major version of the environment template if it\'s not the @Recommended@
-- version. Delete the @Recommended@ version of the environment template if
-- no other major versions or minor versions of the environment template
-- exist. A major version of an environment template is a version that\'s
-- not backward compatible.
--
-- Delete a minor version of an environment template if it /isn\'t/ the
-- @Recommended@ version. Delete a @Recommended@ minor version of the
-- environment template if no other minor versions of the environment
-- template exist. A minor version of an environment template is a version
-- that\'s backward compatible.
module Amazonka.Proton.DeleteEnvironmentTemplateVersion
  ( -- * Creating a Request
    DeleteEnvironmentTemplateVersion (..),
    newDeleteEnvironmentTemplateVersion,

    -- * Request Lenses
    deleteEnvironmentTemplateVersion_majorVersion,
    deleteEnvironmentTemplateVersion_minorVersion,
    deleteEnvironmentTemplateVersion_templateName,

    -- * Destructuring the Response
    DeleteEnvironmentTemplateVersionResponse (..),
    newDeleteEnvironmentTemplateVersionResponse,

    -- * Response Lenses
    deleteEnvironmentTemplateVersionResponse_environmentTemplateVersion,
    deleteEnvironmentTemplateVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEnvironmentTemplateVersion' smart constructor.
data DeleteEnvironmentTemplateVersion = DeleteEnvironmentTemplateVersion'
  { -- | The environment template major version to delete.
    majorVersion :: Prelude.Text,
    -- | The environment template minor version to delete.
    minorVersion :: Prelude.Text,
    -- | The name of the environment template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'majorVersion', 'deleteEnvironmentTemplateVersion_majorVersion' - The environment template major version to delete.
--
-- 'minorVersion', 'deleteEnvironmentTemplateVersion_minorVersion' - The environment template minor version to delete.
--
-- 'templateName', 'deleteEnvironmentTemplateVersion_templateName' - The name of the environment template.
newDeleteEnvironmentTemplateVersion ::
  -- | 'majorVersion'
  Prelude.Text ->
  -- | 'minorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  DeleteEnvironmentTemplateVersion
newDeleteEnvironmentTemplateVersion
  pMajorVersion_
  pMinorVersion_
  pTemplateName_ =
    DeleteEnvironmentTemplateVersion'
      { majorVersion =
          pMajorVersion_,
        minorVersion = pMinorVersion_,
        templateName = pTemplateName_
      }

-- | The environment template major version to delete.
deleteEnvironmentTemplateVersion_majorVersion :: Lens.Lens' DeleteEnvironmentTemplateVersion Prelude.Text
deleteEnvironmentTemplateVersion_majorVersion = Lens.lens (\DeleteEnvironmentTemplateVersion' {majorVersion} -> majorVersion) (\s@DeleteEnvironmentTemplateVersion' {} a -> s {majorVersion = a} :: DeleteEnvironmentTemplateVersion)

-- | The environment template minor version to delete.
deleteEnvironmentTemplateVersion_minorVersion :: Lens.Lens' DeleteEnvironmentTemplateVersion Prelude.Text
deleteEnvironmentTemplateVersion_minorVersion = Lens.lens (\DeleteEnvironmentTemplateVersion' {minorVersion} -> minorVersion) (\s@DeleteEnvironmentTemplateVersion' {} a -> s {minorVersion = a} :: DeleteEnvironmentTemplateVersion)

-- | The name of the environment template.
deleteEnvironmentTemplateVersion_templateName :: Lens.Lens' DeleteEnvironmentTemplateVersion Prelude.Text
deleteEnvironmentTemplateVersion_templateName = Lens.lens (\DeleteEnvironmentTemplateVersion' {templateName} -> templateName) (\s@DeleteEnvironmentTemplateVersion' {} a -> s {templateName = a} :: DeleteEnvironmentTemplateVersion)

instance
  Core.AWSRequest
    DeleteEnvironmentTemplateVersion
  where
  type
    AWSResponse DeleteEnvironmentTemplateVersion =
      DeleteEnvironmentTemplateVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteEnvironmentTemplateVersionResponse'
            Prelude.<$> (x Core..?> "environmentTemplateVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteEnvironmentTemplateVersion
  where
  hashWithSalt
    _salt
    DeleteEnvironmentTemplateVersion' {..} =
      _salt `Prelude.hashWithSalt` majorVersion
        `Prelude.hashWithSalt` minorVersion
        `Prelude.hashWithSalt` templateName

instance
  Prelude.NFData
    DeleteEnvironmentTemplateVersion
  where
  rnf DeleteEnvironmentTemplateVersion' {..} =
    Prelude.rnf majorVersion
      `Prelude.seq` Prelude.rnf minorVersion
      `Prelude.seq` Prelude.rnf templateName

instance
  Core.ToHeaders
    DeleteEnvironmentTemplateVersion
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.DeleteEnvironmentTemplateVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteEnvironmentTemplateVersion where
  toJSON DeleteEnvironmentTemplateVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("majorVersion" Core..= majorVersion),
            Prelude.Just ("minorVersion" Core..= minorVersion),
            Prelude.Just ("templateName" Core..= templateName)
          ]
      )

instance Core.ToPath DeleteEnvironmentTemplateVersion where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    DeleteEnvironmentTemplateVersion
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEnvironmentTemplateVersionResponse' smart constructor.
data DeleteEnvironmentTemplateVersionResponse = DeleteEnvironmentTemplateVersionResponse'
  { -- | The detailed data of the environment template version being deleted.
    environmentTemplateVersion :: Prelude.Maybe EnvironmentTemplateVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEnvironmentTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentTemplateVersion', 'deleteEnvironmentTemplateVersionResponse_environmentTemplateVersion' - The detailed data of the environment template version being deleted.
--
-- 'httpStatus', 'deleteEnvironmentTemplateVersionResponse_httpStatus' - The response's http status code.
newDeleteEnvironmentTemplateVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEnvironmentTemplateVersionResponse
newDeleteEnvironmentTemplateVersionResponse
  pHttpStatus_ =
    DeleteEnvironmentTemplateVersionResponse'
      { environmentTemplateVersion =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The detailed data of the environment template version being deleted.
deleteEnvironmentTemplateVersionResponse_environmentTemplateVersion :: Lens.Lens' DeleteEnvironmentTemplateVersionResponse (Prelude.Maybe EnvironmentTemplateVersion)
deleteEnvironmentTemplateVersionResponse_environmentTemplateVersion = Lens.lens (\DeleteEnvironmentTemplateVersionResponse' {environmentTemplateVersion} -> environmentTemplateVersion) (\s@DeleteEnvironmentTemplateVersionResponse' {} a -> s {environmentTemplateVersion = a} :: DeleteEnvironmentTemplateVersionResponse)

-- | The response's http status code.
deleteEnvironmentTemplateVersionResponse_httpStatus :: Lens.Lens' DeleteEnvironmentTemplateVersionResponse Prelude.Int
deleteEnvironmentTemplateVersionResponse_httpStatus = Lens.lens (\DeleteEnvironmentTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteEnvironmentTemplateVersionResponse' {} a -> s {httpStatus = a} :: DeleteEnvironmentTemplateVersionResponse)

instance
  Prelude.NFData
    DeleteEnvironmentTemplateVersionResponse
  where
  rnf DeleteEnvironmentTemplateVersionResponse' {..} =
    Prelude.rnf environmentTemplateVersion
      `Prelude.seq` Prelude.rnf httpStatus
