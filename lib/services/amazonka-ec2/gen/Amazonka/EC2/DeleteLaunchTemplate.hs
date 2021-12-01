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
-- Module      : Amazonka.EC2.DeleteLaunchTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a launch template. Deleting a launch template deletes all of its
-- versions.
module Amazonka.EC2.DeleteLaunchTemplate
  ( -- * Creating a Request
    DeleteLaunchTemplate (..),
    newDeleteLaunchTemplate,

    -- * Request Lenses
    deleteLaunchTemplate_launchTemplateName,
    deleteLaunchTemplate_launchTemplateId,
    deleteLaunchTemplate_dryRun,

    -- * Destructuring the Response
    DeleteLaunchTemplateResponse (..),
    newDeleteLaunchTemplateResponse,

    -- * Response Lenses
    deleteLaunchTemplateResponse_launchTemplate,
    deleteLaunchTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLaunchTemplate' smart constructor.
data DeleteLaunchTemplate = DeleteLaunchTemplate'
  { -- | The name of the launch template. You must specify either the launch
    -- template ID or launch template name in the request.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the launch template. You must specify either the launch
    -- template ID or launch template name in the request.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLaunchTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateName', 'deleteLaunchTemplate_launchTemplateName' - The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
--
-- 'launchTemplateId', 'deleteLaunchTemplate_launchTemplateId' - The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
--
-- 'dryRun', 'deleteLaunchTemplate_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDeleteLaunchTemplate ::
  DeleteLaunchTemplate
newDeleteLaunchTemplate =
  DeleteLaunchTemplate'
    { launchTemplateName =
        Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      dryRun = Prelude.Nothing
    }

-- | The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
deleteLaunchTemplate_launchTemplateName :: Lens.Lens' DeleteLaunchTemplate (Prelude.Maybe Prelude.Text)
deleteLaunchTemplate_launchTemplateName = Lens.lens (\DeleteLaunchTemplate' {launchTemplateName} -> launchTemplateName) (\s@DeleteLaunchTemplate' {} a -> s {launchTemplateName = a} :: DeleteLaunchTemplate)

-- | The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
deleteLaunchTemplate_launchTemplateId :: Lens.Lens' DeleteLaunchTemplate (Prelude.Maybe Prelude.Text)
deleteLaunchTemplate_launchTemplateId = Lens.lens (\DeleteLaunchTemplate' {launchTemplateId} -> launchTemplateId) (\s@DeleteLaunchTemplate' {} a -> s {launchTemplateId = a} :: DeleteLaunchTemplate)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteLaunchTemplate_dryRun :: Lens.Lens' DeleteLaunchTemplate (Prelude.Maybe Prelude.Bool)
deleteLaunchTemplate_dryRun = Lens.lens (\DeleteLaunchTemplate' {dryRun} -> dryRun) (\s@DeleteLaunchTemplate' {} a -> s {dryRun = a} :: DeleteLaunchTemplate)

instance Core.AWSRequest DeleteLaunchTemplate where
  type
    AWSResponse DeleteLaunchTemplate =
      DeleteLaunchTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLaunchTemplateResponse'
            Prelude.<$> (x Core..@? "launchTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLaunchTemplate where
  hashWithSalt salt' DeleteLaunchTemplate' {..} =
    salt' `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` launchTemplateName

instance Prelude.NFData DeleteLaunchTemplate where
  rnf DeleteLaunchTemplate' {..} =
    Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf launchTemplateId

instance Core.ToHeaders DeleteLaunchTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DeleteLaunchTemplate where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteLaunchTemplate where
  toQuery DeleteLaunchTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DeleteLaunchTemplate" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "LaunchTemplateName" Core.=: launchTemplateName,
        "LaunchTemplateId" Core.=: launchTemplateId,
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newDeleteLaunchTemplateResponse' smart constructor.
data DeleteLaunchTemplateResponse = DeleteLaunchTemplateResponse'
  { -- | Information about the launch template.
    launchTemplate :: Prelude.Maybe LaunchTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLaunchTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplate', 'deleteLaunchTemplateResponse_launchTemplate' - Information about the launch template.
--
-- 'httpStatus', 'deleteLaunchTemplateResponse_httpStatus' - The response's http status code.
newDeleteLaunchTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLaunchTemplateResponse
newDeleteLaunchTemplateResponse pHttpStatus_ =
  DeleteLaunchTemplateResponse'
    { launchTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the launch template.
deleteLaunchTemplateResponse_launchTemplate :: Lens.Lens' DeleteLaunchTemplateResponse (Prelude.Maybe LaunchTemplate)
deleteLaunchTemplateResponse_launchTemplate = Lens.lens (\DeleteLaunchTemplateResponse' {launchTemplate} -> launchTemplate) (\s@DeleteLaunchTemplateResponse' {} a -> s {launchTemplate = a} :: DeleteLaunchTemplateResponse)

-- | The response's http status code.
deleteLaunchTemplateResponse_httpStatus :: Lens.Lens' DeleteLaunchTemplateResponse Prelude.Int
deleteLaunchTemplateResponse_httpStatus = Lens.lens (\DeleteLaunchTemplateResponse' {httpStatus} -> httpStatus) (\s@DeleteLaunchTemplateResponse' {} a -> s {httpStatus = a} :: DeleteLaunchTemplateResponse)

instance Prelude.NFData DeleteLaunchTemplateResponse where
  rnf DeleteLaunchTemplateResponse' {..} =
    Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf httpStatus
