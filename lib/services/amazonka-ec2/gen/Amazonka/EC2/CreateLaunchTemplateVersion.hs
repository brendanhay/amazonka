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
-- Module      : Amazonka.EC2.CreateLaunchTemplateVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of a launch template. You can specify an existing
-- version of launch template from which to base the new version.
--
-- Launch template versions are numbered in the order in which they are
-- created. You cannot specify, change, or replace the numbering of launch
-- template versions.
--
-- Launch templates are immutable; after you create a launch template, you
-- can\'t modify it. Instead, you can create a new version of the launch
-- template that includes any changes you require.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#manage-launch-template-versions Modify a launch template (manage launch template versions)>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.CreateLaunchTemplateVersion
  ( -- * Creating a Request
    CreateLaunchTemplateVersion (..),
    newCreateLaunchTemplateVersion,

    -- * Request Lenses
    createLaunchTemplateVersion_clientToken,
    createLaunchTemplateVersion_dryRun,
    createLaunchTemplateVersion_launchTemplateId,
    createLaunchTemplateVersion_launchTemplateName,
    createLaunchTemplateVersion_sourceVersion,
    createLaunchTemplateVersion_versionDescription,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLaunchTemplateVersion' smart constructor.
data CreateLaunchTemplateVersion = CreateLaunchTemplateVersion'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
    --
    -- Constraint: Maximum 128 ASCII characters.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the launch template.
    --
    -- You must specify either the @LaunchTemplateId@ or the
    -- @LaunchTemplateName@, but not both.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template.
    --
    -- You must specify the @LaunchTemplateName@ or the @LaunchTemplateId@, but
    -- not both.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the launch template version on which to base the
    -- new version. The new version inherits the same launch parameters as the
    -- source version, except for parameters that you specify in
    -- @LaunchTemplateData@. Snapshots applied to the block device mapping are
    -- ignored when creating a new version unless they are explicitly included.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | A description for the version of the launch template.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | The information for the launch template.
    launchTemplateData :: Data.Sensitive RequestLaunchTemplateData
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchTemplateVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createLaunchTemplateVersion_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
--
-- 'dryRun', 'createLaunchTemplateVersion_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'launchTemplateId', 'createLaunchTemplateVersion_launchTemplateId' - The ID of the launch template.
--
-- You must specify either the @LaunchTemplateId@ or the
-- @LaunchTemplateName@, but not both.
--
-- 'launchTemplateName', 'createLaunchTemplateVersion_launchTemplateName' - The name of the launch template.
--
-- You must specify the @LaunchTemplateName@ or the @LaunchTemplateId@, but
-- not both.
--
-- 'sourceVersion', 'createLaunchTemplateVersion_sourceVersion' - The version number of the launch template version on which to base the
-- new version. The new version inherits the same launch parameters as the
-- source version, except for parameters that you specify in
-- @LaunchTemplateData@. Snapshots applied to the block device mapping are
-- ignored when creating a new version unless they are explicitly included.
--
-- 'versionDescription', 'createLaunchTemplateVersion_versionDescription' - A description for the version of the launch template.
--
-- 'launchTemplateData', 'createLaunchTemplateVersion_launchTemplateData' - The information for the launch template.
newCreateLaunchTemplateVersion ::
  -- | 'launchTemplateData'
  RequestLaunchTemplateData ->
  CreateLaunchTemplateVersion
newCreateLaunchTemplateVersion pLaunchTemplateData_ =
  CreateLaunchTemplateVersion'
    { clientToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing,
      sourceVersion = Prelude.Nothing,
      versionDescription = Prelude.Nothing,
      launchTemplateData =
        Data._Sensitive Lens.# pLaunchTemplateData_
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
createLaunchTemplateVersion_clientToken :: Lens.Lens' CreateLaunchTemplateVersion (Prelude.Maybe Prelude.Text)
createLaunchTemplateVersion_clientToken = Lens.lens (\CreateLaunchTemplateVersion' {clientToken} -> clientToken) (\s@CreateLaunchTemplateVersion' {} a -> s {clientToken = a} :: CreateLaunchTemplateVersion)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createLaunchTemplateVersion_dryRun :: Lens.Lens' CreateLaunchTemplateVersion (Prelude.Maybe Prelude.Bool)
createLaunchTemplateVersion_dryRun = Lens.lens (\CreateLaunchTemplateVersion' {dryRun} -> dryRun) (\s@CreateLaunchTemplateVersion' {} a -> s {dryRun = a} :: CreateLaunchTemplateVersion)

-- | The ID of the launch template.
--
-- You must specify either the @LaunchTemplateId@ or the
-- @LaunchTemplateName@, but not both.
createLaunchTemplateVersion_launchTemplateId :: Lens.Lens' CreateLaunchTemplateVersion (Prelude.Maybe Prelude.Text)
createLaunchTemplateVersion_launchTemplateId = Lens.lens (\CreateLaunchTemplateVersion' {launchTemplateId} -> launchTemplateId) (\s@CreateLaunchTemplateVersion' {} a -> s {launchTemplateId = a} :: CreateLaunchTemplateVersion)

-- | The name of the launch template.
--
-- You must specify the @LaunchTemplateName@ or the @LaunchTemplateId@, but
-- not both.
createLaunchTemplateVersion_launchTemplateName :: Lens.Lens' CreateLaunchTemplateVersion (Prelude.Maybe Prelude.Text)
createLaunchTemplateVersion_launchTemplateName = Lens.lens (\CreateLaunchTemplateVersion' {launchTemplateName} -> launchTemplateName) (\s@CreateLaunchTemplateVersion' {} a -> s {launchTemplateName = a} :: CreateLaunchTemplateVersion)

-- | The version number of the launch template version on which to base the
-- new version. The new version inherits the same launch parameters as the
-- source version, except for parameters that you specify in
-- @LaunchTemplateData@. Snapshots applied to the block device mapping are
-- ignored when creating a new version unless they are explicitly included.
createLaunchTemplateVersion_sourceVersion :: Lens.Lens' CreateLaunchTemplateVersion (Prelude.Maybe Prelude.Text)
createLaunchTemplateVersion_sourceVersion = Lens.lens (\CreateLaunchTemplateVersion' {sourceVersion} -> sourceVersion) (\s@CreateLaunchTemplateVersion' {} a -> s {sourceVersion = a} :: CreateLaunchTemplateVersion)

-- | A description for the version of the launch template.
createLaunchTemplateVersion_versionDescription :: Lens.Lens' CreateLaunchTemplateVersion (Prelude.Maybe Prelude.Text)
createLaunchTemplateVersion_versionDescription = Lens.lens (\CreateLaunchTemplateVersion' {versionDescription} -> versionDescription) (\s@CreateLaunchTemplateVersion' {} a -> s {versionDescription = a} :: CreateLaunchTemplateVersion)

-- | The information for the launch template.
createLaunchTemplateVersion_launchTemplateData :: Lens.Lens' CreateLaunchTemplateVersion RequestLaunchTemplateData
createLaunchTemplateVersion_launchTemplateData = Lens.lens (\CreateLaunchTemplateVersion' {launchTemplateData} -> launchTemplateData) (\s@CreateLaunchTemplateVersion' {} a -> s {launchTemplateData = a} :: CreateLaunchTemplateVersion) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateLaunchTemplateVersion where
  type
    AWSResponse CreateLaunchTemplateVersion =
      CreateLaunchTemplateVersionResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateLaunchTemplateVersionResponse'
            Prelude.<$> (x Data..@? "launchTemplateVersion")
            Prelude.<*> (x Data..@? "warning")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLaunchTemplateVersion where
  hashWithSalt _salt CreateLaunchTemplateVersion' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` launchTemplateName
      `Prelude.hashWithSalt` sourceVersion
      `Prelude.hashWithSalt` versionDescription
      `Prelude.hashWithSalt` launchTemplateData

instance Prelude.NFData CreateLaunchTemplateVersion where
  rnf CreateLaunchTemplateVersion' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf versionDescription
      `Prelude.seq` Prelude.rnf launchTemplateData

instance Data.ToHeaders CreateLaunchTemplateVersion where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateLaunchTemplateVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLaunchTemplateVersion where
  toQuery CreateLaunchTemplateVersion' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateLaunchTemplateVersion" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "LaunchTemplateId" Data.=: launchTemplateId,
        "LaunchTemplateName" Data.=: launchTemplateName,
        "SourceVersion" Data.=: sourceVersion,
        "VersionDescription" Data.=: versionDescription,
        "LaunchTemplateData" Data.=: launchTemplateData
      ]

-- | /See:/ 'newCreateLaunchTemplateVersionResponse' smart constructor.
data CreateLaunchTemplateVersionResponse = CreateLaunchTemplateVersionResponse'
  { -- | Information about the launch template version.
    launchTemplateVersion :: Prelude.Maybe LaunchTemplateVersion,
    -- | If the new version of the launch template contains parameters or
    -- parameter combinations that are not valid, an error code and an error
    -- message are returned for each issue that\'s found.
    warning :: Prelude.Maybe ValidationWarning,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateLaunchTemplateVersionResponse
newCreateLaunchTemplateVersionResponse pHttpStatus_ =
  CreateLaunchTemplateVersionResponse'
    { launchTemplateVersion =
        Prelude.Nothing,
      warning = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the launch template version.
createLaunchTemplateVersionResponse_launchTemplateVersion :: Lens.Lens' CreateLaunchTemplateVersionResponse (Prelude.Maybe LaunchTemplateVersion)
createLaunchTemplateVersionResponse_launchTemplateVersion = Lens.lens (\CreateLaunchTemplateVersionResponse' {launchTemplateVersion} -> launchTemplateVersion) (\s@CreateLaunchTemplateVersionResponse' {} a -> s {launchTemplateVersion = a} :: CreateLaunchTemplateVersionResponse)

-- | If the new version of the launch template contains parameters or
-- parameter combinations that are not valid, an error code and an error
-- message are returned for each issue that\'s found.
createLaunchTemplateVersionResponse_warning :: Lens.Lens' CreateLaunchTemplateVersionResponse (Prelude.Maybe ValidationWarning)
createLaunchTemplateVersionResponse_warning = Lens.lens (\CreateLaunchTemplateVersionResponse' {warning} -> warning) (\s@CreateLaunchTemplateVersionResponse' {} a -> s {warning = a} :: CreateLaunchTemplateVersionResponse)

-- | The response's http status code.
createLaunchTemplateVersionResponse_httpStatus :: Lens.Lens' CreateLaunchTemplateVersionResponse Prelude.Int
createLaunchTemplateVersionResponse_httpStatus = Lens.lens (\CreateLaunchTemplateVersionResponse' {httpStatus} -> httpStatus) (\s@CreateLaunchTemplateVersionResponse' {} a -> s {httpStatus = a} :: CreateLaunchTemplateVersionResponse)

instance
  Prelude.NFData
    CreateLaunchTemplateVersionResponse
  where
  rnf CreateLaunchTemplateVersionResponse' {..} =
    Prelude.rnf launchTemplateVersion
      `Prelude.seq` Prelude.rnf warning
      `Prelude.seq` Prelude.rnf httpStatus
