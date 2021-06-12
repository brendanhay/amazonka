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
-- Module      : Network.AWS.EC2.CreateLaunchTemplateVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version for a launch template. You can specify an existing
-- version of launch template from which to base the new version.
--
-- Launch template versions are numbered in the order in which they are
-- created. You cannot specify, change, or replace the numbering of launch
-- template versions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#manage-launch-template-versions Managing launch template versions>in
-- the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.CreateLaunchTemplateVersion
  ( -- * Creating a Request
    CreateLaunchTemplateVersion (..),
    newCreateLaunchTemplateVersion,

    -- * Request Lenses
    createLaunchTemplateVersion_sourceVersion,
    createLaunchTemplateVersion_dryRun,
    createLaunchTemplateVersion_launchTemplateId,
    createLaunchTemplateVersion_launchTemplateName,
    createLaunchTemplateVersion_versionDescription,
    createLaunchTemplateVersion_clientToken,
    createLaunchTemplateVersion_launchTemplateData,

    -- * Destructuring the Response
    CreateLaunchTemplateVersionResponse (..),
    newCreateLaunchTemplateVersionResponse,

    -- * Response Lenses
    createLaunchTemplateVersionResponse_launchTemplateVersion,
    createLaunchTemplateVersionResponse_warning,
    createLaunchTemplateVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLaunchTemplateVersion' smart constructor.
data CreateLaunchTemplateVersion = CreateLaunchTemplateVersion'
  { -- | The version number of the launch template version on which to base the
    -- new version. The new version inherits the same launch parameters as the
    -- source version, except for parameters that you specify in
    -- @LaunchTemplateData@. Snapshots applied to the block device mapping are
    -- ignored when creating a new version unless they are explicitly included.
    sourceVersion :: Core.Maybe Core.Text,
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
    -- | A description for the version of the launch template.
    versionDescription :: Core.Maybe Core.Text,
    -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    --
    -- Constraint: Maximum 128 ASCII characters.
    clientToken :: Core.Maybe Core.Text,
    -- | The information for the launch template.
    launchTemplateData :: RequestLaunchTemplateData
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLaunchTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceVersion', 'createLaunchTemplateVersion_sourceVersion' - The version number of the launch template version on which to base the
-- new version. The new version inherits the same launch parameters as the
-- source version, except for parameters that you specify in
-- @LaunchTemplateData@. Snapshots applied to the block device mapping are
-- ignored when creating a new version unless they are explicitly included.
--
-- 'dryRun', 'createLaunchTemplateVersion_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'launchTemplateId', 'createLaunchTemplateVersion_launchTemplateId' - The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
--
-- 'launchTemplateName', 'createLaunchTemplateVersion_launchTemplateName' - The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
--
-- 'versionDescription', 'createLaunchTemplateVersion_versionDescription' - A description for the version of the launch template.
--
-- 'clientToken', 'createLaunchTemplateVersion_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
--
-- 'launchTemplateData', 'createLaunchTemplateVersion_launchTemplateData' - The information for the launch template.
newCreateLaunchTemplateVersion ::
  -- | 'launchTemplateData'
  RequestLaunchTemplateData ->
  CreateLaunchTemplateVersion
newCreateLaunchTemplateVersion pLaunchTemplateData_ =
  CreateLaunchTemplateVersion'
    { sourceVersion =
        Core.Nothing,
      dryRun = Core.Nothing,
      launchTemplateId = Core.Nothing,
      launchTemplateName = Core.Nothing,
      versionDescription = Core.Nothing,
      clientToken = Core.Nothing,
      launchTemplateData = pLaunchTemplateData_
    }

-- | The version number of the launch template version on which to base the
-- new version. The new version inherits the same launch parameters as the
-- source version, except for parameters that you specify in
-- @LaunchTemplateData@. Snapshots applied to the block device mapping are
-- ignored when creating a new version unless they are explicitly included.
createLaunchTemplateVersion_sourceVersion :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Core.Text)
createLaunchTemplateVersion_sourceVersion = Lens.lens (\CreateLaunchTemplateVersion' {sourceVersion} -> sourceVersion) (\s@CreateLaunchTemplateVersion' {} a -> s {sourceVersion = a} :: CreateLaunchTemplateVersion)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createLaunchTemplateVersion_dryRun :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Core.Bool)
createLaunchTemplateVersion_dryRun = Lens.lens (\CreateLaunchTemplateVersion' {dryRun} -> dryRun) (\s@CreateLaunchTemplateVersion' {} a -> s {dryRun = a} :: CreateLaunchTemplateVersion)

-- | The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
createLaunchTemplateVersion_launchTemplateId :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Core.Text)
createLaunchTemplateVersion_launchTemplateId = Lens.lens (\CreateLaunchTemplateVersion' {launchTemplateId} -> launchTemplateId) (\s@CreateLaunchTemplateVersion' {} a -> s {launchTemplateId = a} :: CreateLaunchTemplateVersion)

-- | The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
createLaunchTemplateVersion_launchTemplateName :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Core.Text)
createLaunchTemplateVersion_launchTemplateName = Lens.lens (\CreateLaunchTemplateVersion' {launchTemplateName} -> launchTemplateName) (\s@CreateLaunchTemplateVersion' {} a -> s {launchTemplateName = a} :: CreateLaunchTemplateVersion)

-- | A description for the version of the launch template.
createLaunchTemplateVersion_versionDescription :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Core.Text)
createLaunchTemplateVersion_versionDescription = Lens.lens (\CreateLaunchTemplateVersion' {versionDescription} -> versionDescription) (\s@CreateLaunchTemplateVersion' {} a -> s {versionDescription = a} :: CreateLaunchTemplateVersion)

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
createLaunchTemplateVersion_clientToken :: Lens.Lens' CreateLaunchTemplateVersion (Core.Maybe Core.Text)
createLaunchTemplateVersion_clientToken = Lens.lens (\CreateLaunchTemplateVersion' {clientToken} -> clientToken) (\s@CreateLaunchTemplateVersion' {} a -> s {clientToken = a} :: CreateLaunchTemplateVersion)

-- | The information for the launch template.
createLaunchTemplateVersion_launchTemplateData :: Lens.Lens' CreateLaunchTemplateVersion RequestLaunchTemplateData
createLaunchTemplateVersion_launchTemplateData = Lens.lens (\CreateLaunchTemplateVersion' {launchTemplateData} -> launchTemplateData) (\s@CreateLaunchTemplateVersion' {} a -> s {launchTemplateData = a} :: CreateLaunchTemplateVersion)

instance Core.AWSRequest CreateLaunchTemplateVersion where
  type
    AWSResponse CreateLaunchTemplateVersion =
      CreateLaunchTemplateVersionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateLaunchTemplateVersionResponse'
            Core.<$> (x Core..@? "launchTemplateVersion")
            Core.<*> (x Core..@? "warning")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateLaunchTemplateVersion

instance Core.NFData CreateLaunchTemplateVersion

instance Core.ToHeaders CreateLaunchTemplateVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateLaunchTemplateVersion where
  toPath = Core.const "/"

instance Core.ToQuery CreateLaunchTemplateVersion where
  toQuery CreateLaunchTemplateVersion' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateLaunchTemplateVersion" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "SourceVersion" Core.=: sourceVersion,
        "DryRun" Core.=: dryRun,
        "LaunchTemplateId" Core.=: launchTemplateId,
        "LaunchTemplateName" Core.=: launchTemplateName,
        "VersionDescription" Core.=: versionDescription,
        "ClientToken" Core.=: clientToken,
        "LaunchTemplateData" Core.=: launchTemplateData
      ]

-- | /See:/ 'newCreateLaunchTemplateVersionResponse' smart constructor.
data CreateLaunchTemplateVersionResponse = CreateLaunchTemplateVersionResponse'
  { -- | Information about the launch template version.
    launchTemplateVersion :: Core.Maybe LaunchTemplateVersion,
    -- | If the new version of the launch template contains parameters or
    -- parameter combinations that are not valid, an error code and an error
    -- message are returned for each issue that\'s found.
    warning :: Core.Maybe ValidationWarning,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateLaunchTemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplateVersion', 'createLaunchTemplateVersionResponse_launchTemplateVersion' - Information about the launch template version.
--
-- 'warning', 'createLaunchTemplateVersionResponse_warning' - If the new version of the launch template contains parameters or
-- parameter combinations that are not valid, an error code and an error
-- message are returned for each issue that\'s found.
--
-- 'httpStatus', 'createLaunchTemplateVersionResponse_httpStatus' - The response's http status code.
newCreateLaunchTemplateVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateLaunchTemplateVersionResponse
newCreateLaunchTemplateVersionResponse pHttpStatus_ =
  CreateLaunchTemplateVersionResponse'
    { launchTemplateVersion =
        Core.Nothing,
      warning = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the launch template version.
createLaunchTemplateVersionResponse_launchTemplateVersion :: Lens.Lens' CreateLaunchTemplateVersionResponse (Core.Maybe LaunchTemplateVersion)
createLaunchTemplateVersionResponse_launchTemplateVersion = Lens.lens (\CreateLaunchTemplateVersionResponse' {launchTemplateVersion} -> launchTemplateVersion) (\s@CreateLaunchTemplateVersionResponse' {} a -> s {launchTemplateVersion = a} :: CreateLaunchTemplateVersionResponse)

-- | If the new version of the launch template contains parameters or
-- parameter combinations that are not valid, an error code and an error
-- message are returned for each issue that\'s found.
createLaunchTemplateVersionResponse_warning :: Lens.Lens' CreateLaunchTemplateVersionResponse (Core.Maybe ValidationWarning)
createLaunchTemplateVersionResponse_warning = Lens.lens (\CreateLaunchTemplateVersionResponse' {warning} -> warning) (\s@CreateLaunchTemplateVersionResponse' {} a -> s {warning = a} :: CreateLaunchTemplateVersionResponse)

-- | The response's http status code.
createLaunchTemplateVersionResponse_httpStatus :: Lens.Lens' CreateLaunchTemplateVersionResponse Core.Int
createLaunchTemplateVersionResponse_httpStatus = Lens.lens (\CreateLaunchTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@CreateLaunchTemplateVersionResponse' {} a -> s {httpStatus = a} :: CreateLaunchTemplateVersionResponse)

instance
  Core.NFData
    CreateLaunchTemplateVersionResponse
