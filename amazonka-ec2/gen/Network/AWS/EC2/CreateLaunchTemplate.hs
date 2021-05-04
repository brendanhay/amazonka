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
-- Module      : Network.AWS.EC2.CreateLaunchTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a launch template. A launch template contains the parameters to
-- launch an instance. When you launch an instance using RunInstances, you
-- can specify a launch template instead of providing the launch parameters
-- in the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launching an instance from a launch template>in
-- the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.CreateLaunchTemplate
  ( -- * Creating a Request
    CreateLaunchTemplate (..),
    newCreateLaunchTemplate,

    -- * Request Lenses
    createLaunchTemplate_tagSpecifications,
    createLaunchTemplate_dryRun,
    createLaunchTemplate_versionDescription,
    createLaunchTemplate_clientToken,
    createLaunchTemplate_launchTemplateName,
    createLaunchTemplate_launchTemplateData,

    -- * Destructuring the Response
    CreateLaunchTemplateResponse (..),
    newCreateLaunchTemplateResponse,

    -- * Response Lenses
    createLaunchTemplateResponse_launchTemplate,
    createLaunchTemplateResponse_warning,
    createLaunchTemplateResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLaunchTemplate' smart constructor.
data CreateLaunchTemplate = CreateLaunchTemplate'
  { -- | The tags to apply to the launch template during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | A description for the first version of the launch template.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    --
    -- Constraint: Maximum 128 ASCII characters.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A name for the launch template.
    launchTemplateName :: Prelude.Text,
    -- | The information for the launch template.
    launchTemplateData :: RequestLaunchTemplateData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createLaunchTemplate_tagSpecifications' - The tags to apply to the launch template during creation.
--
-- 'dryRun', 'createLaunchTemplate_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'versionDescription', 'createLaunchTemplate_versionDescription' - A description for the first version of the launch template.
--
-- 'clientToken', 'createLaunchTemplate_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
--
-- 'launchTemplateName', 'createLaunchTemplate_launchTemplateName' - A name for the launch template.
--
-- 'launchTemplateData', 'createLaunchTemplate_launchTemplateData' - The information for the launch template.
newCreateLaunchTemplate ::
  -- | 'launchTemplateName'
  Prelude.Text ->
  -- | 'launchTemplateData'
  RequestLaunchTemplateData ->
  CreateLaunchTemplate
newCreateLaunchTemplate
  pLaunchTemplateName_
  pLaunchTemplateData_ =
    CreateLaunchTemplate'
      { tagSpecifications =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        versionDescription = Prelude.Nothing,
        clientToken = Prelude.Nothing,
        launchTemplateName = pLaunchTemplateName_,
        launchTemplateData = pLaunchTemplateData_
      }

-- | The tags to apply to the launch template during creation.
createLaunchTemplate_tagSpecifications :: Lens.Lens' CreateLaunchTemplate (Prelude.Maybe [TagSpecification])
createLaunchTemplate_tagSpecifications = Lens.lens (\CreateLaunchTemplate' {tagSpecifications} -> tagSpecifications) (\s@CreateLaunchTemplate' {} a -> s {tagSpecifications = a} :: CreateLaunchTemplate) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createLaunchTemplate_dryRun :: Lens.Lens' CreateLaunchTemplate (Prelude.Maybe Prelude.Bool)
createLaunchTemplate_dryRun = Lens.lens (\CreateLaunchTemplate' {dryRun} -> dryRun) (\s@CreateLaunchTemplate' {} a -> s {dryRun = a} :: CreateLaunchTemplate)

-- | A description for the first version of the launch template.
createLaunchTemplate_versionDescription :: Lens.Lens' CreateLaunchTemplate (Prelude.Maybe Prelude.Text)
createLaunchTemplate_versionDescription = Lens.lens (\CreateLaunchTemplate' {versionDescription} -> versionDescription) (\s@CreateLaunchTemplate' {} a -> s {versionDescription = a} :: CreateLaunchTemplate)

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
createLaunchTemplate_clientToken :: Lens.Lens' CreateLaunchTemplate (Prelude.Maybe Prelude.Text)
createLaunchTemplate_clientToken = Lens.lens (\CreateLaunchTemplate' {clientToken} -> clientToken) (\s@CreateLaunchTemplate' {} a -> s {clientToken = a} :: CreateLaunchTemplate)

-- | A name for the launch template.
createLaunchTemplate_launchTemplateName :: Lens.Lens' CreateLaunchTemplate Prelude.Text
createLaunchTemplate_launchTemplateName = Lens.lens (\CreateLaunchTemplate' {launchTemplateName} -> launchTemplateName) (\s@CreateLaunchTemplate' {} a -> s {launchTemplateName = a} :: CreateLaunchTemplate)

-- | The information for the launch template.
createLaunchTemplate_launchTemplateData :: Lens.Lens' CreateLaunchTemplate RequestLaunchTemplateData
createLaunchTemplate_launchTemplateData = Lens.lens (\CreateLaunchTemplate' {launchTemplateData} -> launchTemplateData) (\s@CreateLaunchTemplate' {} a -> s {launchTemplateData = a} :: CreateLaunchTemplate)

instance Prelude.AWSRequest CreateLaunchTemplate where
  type
    Rs CreateLaunchTemplate =
      CreateLaunchTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateLaunchTemplateResponse'
            Prelude.<$> (x Prelude..@? "launchTemplate")
            Prelude.<*> (x Prelude..@? "warning")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLaunchTemplate

instance Prelude.NFData CreateLaunchTemplate

instance Prelude.ToHeaders CreateLaunchTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CreateLaunchTemplate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreateLaunchTemplate where
  toQuery CreateLaunchTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CreateLaunchTemplate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "VersionDescription" Prelude.=: versionDescription,
        "ClientToken" Prelude.=: clientToken,
        "LaunchTemplateName" Prelude.=: launchTemplateName,
        "LaunchTemplateData" Prelude.=: launchTemplateData
      ]

-- | /See:/ 'newCreateLaunchTemplateResponse' smart constructor.
data CreateLaunchTemplateResponse = CreateLaunchTemplateResponse'
  { -- | Information about the launch template.
    launchTemplate :: Prelude.Maybe LaunchTemplate,
    -- | If the launch template contains parameters or parameter combinations
    -- that are not valid, an error code and an error message are returned for
    -- each issue that\'s found.
    warning :: Prelude.Maybe ValidationWarning,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchTemplate', 'createLaunchTemplateResponse_launchTemplate' - Information about the launch template.
--
-- 'warning', 'createLaunchTemplateResponse_warning' - If the launch template contains parameters or parameter combinations
-- that are not valid, an error code and an error message are returned for
-- each issue that\'s found.
--
-- 'httpStatus', 'createLaunchTemplateResponse_httpStatus' - The response's http status code.
newCreateLaunchTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLaunchTemplateResponse
newCreateLaunchTemplateResponse pHttpStatus_ =
  CreateLaunchTemplateResponse'
    { launchTemplate =
        Prelude.Nothing,
      warning = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the launch template.
createLaunchTemplateResponse_launchTemplate :: Lens.Lens' CreateLaunchTemplateResponse (Prelude.Maybe LaunchTemplate)
createLaunchTemplateResponse_launchTemplate = Lens.lens (\CreateLaunchTemplateResponse' {launchTemplate} -> launchTemplate) (\s@CreateLaunchTemplateResponse' {} a -> s {launchTemplate = a} :: CreateLaunchTemplateResponse)

-- | If the launch template contains parameters or parameter combinations
-- that are not valid, an error code and an error message are returned for
-- each issue that\'s found.
createLaunchTemplateResponse_warning :: Lens.Lens' CreateLaunchTemplateResponse (Prelude.Maybe ValidationWarning)
createLaunchTemplateResponse_warning = Lens.lens (\CreateLaunchTemplateResponse' {warning} -> warning) (\s@CreateLaunchTemplateResponse' {} a -> s {warning = a} :: CreateLaunchTemplateResponse)

-- | The response's http status code.
createLaunchTemplateResponse_httpStatus :: Lens.Lens' CreateLaunchTemplateResponse Prelude.Int
createLaunchTemplateResponse_httpStatus = Lens.lens (\CreateLaunchTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateLaunchTemplateResponse' {} a -> s {httpStatus = a} :: CreateLaunchTemplateResponse)

instance Prelude.NFData CreateLaunchTemplateResponse
