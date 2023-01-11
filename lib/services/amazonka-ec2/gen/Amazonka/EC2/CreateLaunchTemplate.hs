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
-- Module      : Amazonka.EC2.CreateLaunchTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a launch template.
--
-- A launch template contains the parameters to launch an instance. When
-- you launch an instance using RunInstances, you can specify a launch
-- template instead of providing the launch parameters in the request. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html Launch an instance from a launch template>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- If you want to clone an existing launch template as the basis for
-- creating a new launch template, you can use the Amazon EC2 console. The
-- API, SDKs, and CLI do not support cloning a template. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-launch-templates.html#create-launch-template-from-existing-launch-template Create a launch template from an existing launch template>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Amazonka.EC2.CreateLaunchTemplate
  ( -- * Creating a Request
    CreateLaunchTemplate (..),
    newCreateLaunchTemplate,

    -- * Request Lenses
    createLaunchTemplate_clientToken,
    createLaunchTemplate_dryRun,
    createLaunchTemplate_tagSpecifications,
    createLaunchTemplate_versionDescription,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLaunchTemplate' smart constructor.
data CreateLaunchTemplate = CreateLaunchTemplate'
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
    -- | The tags to apply to the launch template on creation. To tag the launch
    -- template, the resource type must be @launch-template@.
    --
    -- To specify the tags for the resources that are created when an instance
    -- is launched, you must use the @TagSpecifications@ parameter in the
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestLaunchTemplateData.html launch template data>
    -- structure.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | A description for the first version of the launch template.
    versionDescription :: Prelude.Maybe Prelude.Text,
    -- | A name for the launch template.
    launchTemplateName :: Prelude.Text,
    -- | The information for the launch template.
    launchTemplateData :: Data.Sensitive RequestLaunchTemplateData
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createLaunchTemplate_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
--
-- 'dryRun', 'createLaunchTemplate_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'tagSpecifications', 'createLaunchTemplate_tagSpecifications' - The tags to apply to the launch template on creation. To tag the launch
-- template, the resource type must be @launch-template@.
--
-- To specify the tags for the resources that are created when an instance
-- is launched, you must use the @TagSpecifications@ parameter in the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestLaunchTemplateData.html launch template data>
-- structure.
--
-- 'versionDescription', 'createLaunchTemplate_versionDescription' - A description for the first version of the launch template.
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
      { clientToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        tagSpecifications = Prelude.Nothing,
        versionDescription = Prelude.Nothing,
        launchTemplateName = pLaunchTemplateName_,
        launchTemplateData =
          Data._Sensitive Lens.# pLaunchTemplateData_
      }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
createLaunchTemplate_clientToken :: Lens.Lens' CreateLaunchTemplate (Prelude.Maybe Prelude.Text)
createLaunchTemplate_clientToken = Lens.lens (\CreateLaunchTemplate' {clientToken} -> clientToken) (\s@CreateLaunchTemplate' {} a -> s {clientToken = a} :: CreateLaunchTemplate)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createLaunchTemplate_dryRun :: Lens.Lens' CreateLaunchTemplate (Prelude.Maybe Prelude.Bool)
createLaunchTemplate_dryRun = Lens.lens (\CreateLaunchTemplate' {dryRun} -> dryRun) (\s@CreateLaunchTemplate' {} a -> s {dryRun = a} :: CreateLaunchTemplate)

-- | The tags to apply to the launch template on creation. To tag the launch
-- template, the resource type must be @launch-template@.
--
-- To specify the tags for the resources that are created when an instance
-- is launched, you must use the @TagSpecifications@ parameter in the
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RequestLaunchTemplateData.html launch template data>
-- structure.
createLaunchTemplate_tagSpecifications :: Lens.Lens' CreateLaunchTemplate (Prelude.Maybe [TagSpecification])
createLaunchTemplate_tagSpecifications = Lens.lens (\CreateLaunchTemplate' {tagSpecifications} -> tagSpecifications) (\s@CreateLaunchTemplate' {} a -> s {tagSpecifications = a} :: CreateLaunchTemplate) Prelude.. Lens.mapping Lens.coerced

-- | A description for the first version of the launch template.
createLaunchTemplate_versionDescription :: Lens.Lens' CreateLaunchTemplate (Prelude.Maybe Prelude.Text)
createLaunchTemplate_versionDescription = Lens.lens (\CreateLaunchTemplate' {versionDescription} -> versionDescription) (\s@CreateLaunchTemplate' {} a -> s {versionDescription = a} :: CreateLaunchTemplate)

-- | A name for the launch template.
createLaunchTemplate_launchTemplateName :: Lens.Lens' CreateLaunchTemplate Prelude.Text
createLaunchTemplate_launchTemplateName = Lens.lens (\CreateLaunchTemplate' {launchTemplateName} -> launchTemplateName) (\s@CreateLaunchTemplate' {} a -> s {launchTemplateName = a} :: CreateLaunchTemplate)

-- | The information for the launch template.
createLaunchTemplate_launchTemplateData :: Lens.Lens' CreateLaunchTemplate RequestLaunchTemplateData
createLaunchTemplate_launchTemplateData = Lens.lens (\CreateLaunchTemplate' {launchTemplateData} -> launchTemplateData) (\s@CreateLaunchTemplate' {} a -> s {launchTemplateData = a} :: CreateLaunchTemplate) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateLaunchTemplate where
  type
    AWSResponse CreateLaunchTemplate =
      CreateLaunchTemplateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateLaunchTemplateResponse'
            Prelude.<$> (x Data..@? "launchTemplate")
            Prelude.<*> (x Data..@? "warning")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLaunchTemplate where
  hashWithSalt _salt CreateLaunchTemplate' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` versionDescription
      `Prelude.hashWithSalt` launchTemplateName
      `Prelude.hashWithSalt` launchTemplateData

instance Prelude.NFData CreateLaunchTemplate where
  rnf CreateLaunchTemplate' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf versionDescription
      `Prelude.seq` Prelude.rnf launchTemplateName
      `Prelude.seq` Prelude.rnf launchTemplateData

instance Data.ToHeaders CreateLaunchTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateLaunchTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLaunchTemplate where
  toQuery CreateLaunchTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateLaunchTemplate" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "VersionDescription" Data.=: versionDescription,
        "LaunchTemplateName" Data.=: launchTemplateName,
        "LaunchTemplateData" Data.=: launchTemplateData
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreateLaunchTemplateResponse where
  rnf CreateLaunchTemplateResponse' {..} =
    Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf warning
      `Prelude.seq` Prelude.rnf httpStatus
