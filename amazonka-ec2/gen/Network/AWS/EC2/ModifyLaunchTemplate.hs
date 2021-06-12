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
-- Module      : Network.AWS.EC2.ModifyLaunchTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a launch template. You can specify which version of the launch
-- template to set as the default version. When launching an instance, the
-- default version applies when a launch template version is not specified.
module Network.AWS.EC2.ModifyLaunchTemplate
  ( -- * Creating a Request
    ModifyLaunchTemplate (..),
    newModifyLaunchTemplate,

    -- * Request Lenses
    modifyLaunchTemplate_defaultVersion,
    modifyLaunchTemplate_dryRun,
    modifyLaunchTemplate_launchTemplateId,
    modifyLaunchTemplate_launchTemplateName,
    modifyLaunchTemplate_clientToken,

    -- * Destructuring the Response
    ModifyLaunchTemplateResponse (..),
    newModifyLaunchTemplateResponse,

    -- * Response Lenses
    modifyLaunchTemplateResponse_launchTemplate,
    modifyLaunchTemplateResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyLaunchTemplate' smart constructor.
data ModifyLaunchTemplate = ModifyLaunchTemplate'
  { -- | The version number of the launch template to set as the default version.
    defaultVersion :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the launch template. You must specify either the launch
    -- template ID or launch template name in the request.
    launchTemplateId :: Core.Maybe Core.Text,
    -- | The name of the launch template. You must specify either the launch
    -- template ID or launch template name in the request.
    launchTemplateName :: Core.Maybe Core.Text,
    -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    --
    -- Constraint: Maximum 128 ASCII characters.
    clientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyLaunchTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultVersion', 'modifyLaunchTemplate_defaultVersion' - The version number of the launch template to set as the default version.
--
-- 'dryRun', 'modifyLaunchTemplate_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'launchTemplateId', 'modifyLaunchTemplate_launchTemplateId' - The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
--
-- 'launchTemplateName', 'modifyLaunchTemplate_launchTemplateName' - The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
--
-- 'clientToken', 'modifyLaunchTemplate_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
newModifyLaunchTemplate ::
  ModifyLaunchTemplate
newModifyLaunchTemplate =
  ModifyLaunchTemplate'
    { defaultVersion =
        Core.Nothing,
      dryRun = Core.Nothing,
      launchTemplateId = Core.Nothing,
      launchTemplateName = Core.Nothing,
      clientToken = Core.Nothing
    }

-- | The version number of the launch template to set as the default version.
modifyLaunchTemplate_defaultVersion :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Core.Text)
modifyLaunchTemplate_defaultVersion = Lens.lens (\ModifyLaunchTemplate' {defaultVersion} -> defaultVersion) (\s@ModifyLaunchTemplate' {} a -> s {defaultVersion = a} :: ModifyLaunchTemplate)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyLaunchTemplate_dryRun :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Core.Bool)
modifyLaunchTemplate_dryRun = Lens.lens (\ModifyLaunchTemplate' {dryRun} -> dryRun) (\s@ModifyLaunchTemplate' {} a -> s {dryRun = a} :: ModifyLaunchTemplate)

-- | The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
modifyLaunchTemplate_launchTemplateId :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Core.Text)
modifyLaunchTemplate_launchTemplateId = Lens.lens (\ModifyLaunchTemplate' {launchTemplateId} -> launchTemplateId) (\s@ModifyLaunchTemplate' {} a -> s {launchTemplateId = a} :: ModifyLaunchTemplate)

-- | The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
modifyLaunchTemplate_launchTemplateName :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Core.Text)
modifyLaunchTemplate_launchTemplateName = Lens.lens (\ModifyLaunchTemplate' {launchTemplateName} -> launchTemplateName) (\s@ModifyLaunchTemplate' {} a -> s {launchTemplateName = a} :: ModifyLaunchTemplate)

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
modifyLaunchTemplate_clientToken :: Lens.Lens' ModifyLaunchTemplate (Core.Maybe Core.Text)
modifyLaunchTemplate_clientToken = Lens.lens (\ModifyLaunchTemplate' {clientToken} -> clientToken) (\s@ModifyLaunchTemplate' {} a -> s {clientToken = a} :: ModifyLaunchTemplate)

instance Core.AWSRequest ModifyLaunchTemplate where
  type
    AWSResponse ModifyLaunchTemplate =
      ModifyLaunchTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyLaunchTemplateResponse'
            Core.<$> (x Core..@? "launchTemplate")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyLaunchTemplate

instance Core.NFData ModifyLaunchTemplate

instance Core.ToHeaders ModifyLaunchTemplate where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyLaunchTemplate where
  toPath = Core.const "/"

instance Core.ToQuery ModifyLaunchTemplate where
  toQuery ModifyLaunchTemplate' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyLaunchTemplate" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "SetDefaultVersion" Core.=: defaultVersion,
        "DryRun" Core.=: dryRun,
        "LaunchTemplateId" Core.=: launchTemplateId,
        "LaunchTemplateName" Core.=: launchTemplateName,
        "ClientToken" Core.=: clientToken
      ]

-- | /See:/ 'newModifyLaunchTemplateResponse' smart constructor.
data ModifyLaunchTemplateResponse = ModifyLaunchTemplateResponse'
  { -- | Information about the launch template.
    launchTemplate :: Core.Maybe LaunchTemplate,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyLaunchTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplate', 'modifyLaunchTemplateResponse_launchTemplate' - Information about the launch template.
--
-- 'httpStatus', 'modifyLaunchTemplateResponse_httpStatus' - The response's http status code.
newModifyLaunchTemplateResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyLaunchTemplateResponse
newModifyLaunchTemplateResponse pHttpStatus_ =
  ModifyLaunchTemplateResponse'
    { launchTemplate =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the launch template.
modifyLaunchTemplateResponse_launchTemplate :: Lens.Lens' ModifyLaunchTemplateResponse (Core.Maybe LaunchTemplate)
modifyLaunchTemplateResponse_launchTemplate = Lens.lens (\ModifyLaunchTemplateResponse' {launchTemplate} -> launchTemplate) (\s@ModifyLaunchTemplateResponse' {} a -> s {launchTemplate = a} :: ModifyLaunchTemplateResponse)

-- | The response's http status code.
modifyLaunchTemplateResponse_httpStatus :: Lens.Lens' ModifyLaunchTemplateResponse Core.Int
modifyLaunchTemplateResponse_httpStatus = Lens.lens (\ModifyLaunchTemplateResponse' {httpStatus} -> httpStatus) (\s@ModifyLaunchTemplateResponse' {} a -> s {httpStatus = a} :: ModifyLaunchTemplateResponse)

instance Core.NFData ModifyLaunchTemplateResponse
