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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyLaunchTemplate' smart constructor.
data ModifyLaunchTemplate = ModifyLaunchTemplate'
  { -- | The version number of the launch template to set as the default version.
    defaultVersion :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the launch template. You must specify either the launch
    -- template ID or launch template name in the request.
    launchTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The name of the launch template. You must specify either the launch
    -- template ID or launch template name in the request.
    launchTemplateName :: Prelude.Maybe Prelude.Text,
    -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    --
    -- Constraint: Maximum 128 ASCII characters.
    clientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing,
      clientToken = Prelude.Nothing
    }

-- | The version number of the launch template to set as the default version.
modifyLaunchTemplate_defaultVersion :: Lens.Lens' ModifyLaunchTemplate (Prelude.Maybe Prelude.Text)
modifyLaunchTemplate_defaultVersion = Lens.lens (\ModifyLaunchTemplate' {defaultVersion} -> defaultVersion) (\s@ModifyLaunchTemplate' {} a -> s {defaultVersion = a} :: ModifyLaunchTemplate)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyLaunchTemplate_dryRun :: Lens.Lens' ModifyLaunchTemplate (Prelude.Maybe Prelude.Bool)
modifyLaunchTemplate_dryRun = Lens.lens (\ModifyLaunchTemplate' {dryRun} -> dryRun) (\s@ModifyLaunchTemplate' {} a -> s {dryRun = a} :: ModifyLaunchTemplate)

-- | The ID of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
modifyLaunchTemplate_launchTemplateId :: Lens.Lens' ModifyLaunchTemplate (Prelude.Maybe Prelude.Text)
modifyLaunchTemplate_launchTemplateId = Lens.lens (\ModifyLaunchTemplate' {launchTemplateId} -> launchTemplateId) (\s@ModifyLaunchTemplate' {} a -> s {launchTemplateId = a} :: ModifyLaunchTemplate)

-- | The name of the launch template. You must specify either the launch
-- template ID or launch template name in the request.
modifyLaunchTemplate_launchTemplateName :: Lens.Lens' ModifyLaunchTemplate (Prelude.Maybe Prelude.Text)
modifyLaunchTemplate_launchTemplateName = Lens.lens (\ModifyLaunchTemplate' {launchTemplateName} -> launchTemplateName) (\s@ModifyLaunchTemplate' {} a -> s {launchTemplateName = a} :: ModifyLaunchTemplate)

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
modifyLaunchTemplate_clientToken :: Lens.Lens' ModifyLaunchTemplate (Prelude.Maybe Prelude.Text)
modifyLaunchTemplate_clientToken = Lens.lens (\ModifyLaunchTemplate' {clientToken} -> clientToken) (\s@ModifyLaunchTemplate' {} a -> s {clientToken = a} :: ModifyLaunchTemplate)

instance Prelude.AWSRequest ModifyLaunchTemplate where
  type
    Rs ModifyLaunchTemplate =
      ModifyLaunchTemplateResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyLaunchTemplateResponse'
            Prelude.<$> (x Prelude..@? "launchTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyLaunchTemplate

instance Prelude.NFData ModifyLaunchTemplate

instance Prelude.ToHeaders ModifyLaunchTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyLaunchTemplate where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyLaunchTemplate where
  toQuery ModifyLaunchTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyLaunchTemplate" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "SetDefaultVersion" Prelude.=: defaultVersion,
        "DryRun" Prelude.=: dryRun,
        "LaunchTemplateId" Prelude.=: launchTemplateId,
        "LaunchTemplateName" Prelude.=: launchTemplateName,
        "ClientToken" Prelude.=: clientToken
      ]

-- | /See:/ 'newModifyLaunchTemplateResponse' smart constructor.
data ModifyLaunchTemplateResponse = ModifyLaunchTemplateResponse'
  { -- | Information about the launch template.
    launchTemplate :: Prelude.Maybe LaunchTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ModifyLaunchTemplateResponse
newModifyLaunchTemplateResponse pHttpStatus_ =
  ModifyLaunchTemplateResponse'
    { launchTemplate =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the launch template.
modifyLaunchTemplateResponse_launchTemplate :: Lens.Lens' ModifyLaunchTemplateResponse (Prelude.Maybe LaunchTemplate)
modifyLaunchTemplateResponse_launchTemplate = Lens.lens (\ModifyLaunchTemplateResponse' {launchTemplate} -> launchTemplate) (\s@ModifyLaunchTemplateResponse' {} a -> s {launchTemplate = a} :: ModifyLaunchTemplateResponse)

-- | The response's http status code.
modifyLaunchTemplateResponse_httpStatus :: Lens.Lens' ModifyLaunchTemplateResponse Prelude.Int
modifyLaunchTemplateResponse_httpStatus = Lens.lens (\ModifyLaunchTemplateResponse' {httpStatus} -> httpStatus) (\s@ModifyLaunchTemplateResponse' {} a -> s {httpStatus = a} :: ModifyLaunchTemplateResponse)

instance Prelude.NFData ModifyLaunchTemplateResponse
