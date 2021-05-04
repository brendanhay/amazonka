{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DeleteLaunchTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a launch template. Deleting a launch template deletes all of its
-- versions.
module Network.AWS.EC2.DeleteLaunchTemplate
  ( -- * Creating a Request
    DeleteLaunchTemplate (..),
    newDeleteLaunchTemplate,

    -- * Request Lenses
    deleteLaunchTemplate_dryRun,
    deleteLaunchTemplate_launchTemplateId,
    deleteLaunchTemplate_launchTemplateName,

    -- * Destructuring the Response
    DeleteLaunchTemplateResponse (..),
    newDeleteLaunchTemplateResponse,

    -- * Response Lenses
    deleteLaunchTemplateResponse_launchTemplate,
    deleteLaunchTemplateResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLaunchTemplate' smart constructor.
data DeleteLaunchTemplate = DeleteLaunchTemplate'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the launch template. You must specify either the launch
    -- template ID or launch template name in the request.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template. You must specify either the launch
    -- template ID or launch template name in the request.
    launchTemplateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLaunchTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteLaunchTemplate_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'launchTemplateId', 'deleteLaunchTemplate_launchTemplateId' - The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
--
-- 'launchTemplateName', 'deleteLaunchTemplate_launchTemplateName' - The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
newDeleteLaunchTemplate ::
  DeleteLaunchTemplate
newDeleteLaunchTemplate =
  DeleteLaunchTemplate'
    { dryRun = Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteLaunchTemplate_dryRun :: Lens.Lens' DeleteLaunchTemplate (Prelude.Maybe Prelude.Bool)
deleteLaunchTemplate_dryRun = Lens.lens (\DeleteLaunchTemplate' {dryRun} -> dryRun) (\s@DeleteLaunchTemplate' {} a -> s {dryRun = a} :: DeleteLaunchTemplate)

-- | The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
deleteLaunchTemplate_launchTemplateId :: Lens.Lens' DeleteLaunchTemplate (Prelude.Maybe Prelude.Text)
deleteLaunchTemplate_launchTemplateId = Lens.lens (\DeleteLaunchTemplate' {launchTemplateId} -> launchTemplateId) (\s@DeleteLaunchTemplate' {} a -> s {launchTemplateId = a} :: DeleteLaunchTemplate)

-- | The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
deleteLaunchTemplate_launchTemplateName :: Lens.Lens' DeleteLaunchTemplate (Prelude.Maybe Prelude.Text)
deleteLaunchTemplate_launchTemplateName = Lens.lens (\DeleteLaunchTemplate' {launchTemplateName} -> launchTemplateName) (\s@DeleteLaunchTemplate' {} a -> s {launchTemplateName = a} :: DeleteLaunchTemplate)

instance Prelude.AWSRequest DeleteLaunchTemplate where
  type
    Rs DeleteLaunchTemplate =
      DeleteLaunchTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteLaunchTemplateResponse'
            Prelude.<$> (x Prelude..@? "launchTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLaunchTemplate

instance Prelude.NFData DeleteLaunchTemplate

instance Prelude.ToHeaders DeleteLaunchTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteLaunchTemplate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLaunchTemplate where
  toQuery DeleteLaunchTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteLaunchTemplate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "LaunchTemplateId" Prelude.=: launchTemplateId,
        "LaunchTemplateName" Prelude.=: launchTemplateName
      ]

-- | /See:/ 'newDeleteLaunchTemplateResponse' smart constructor.
data DeleteLaunchTemplateResponse = DeleteLaunchTemplateResponse'
  { -- | Information about the launch template.
    launchTemplate :: Prelude.Maybe LaunchTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteLaunchTemplateResponse
