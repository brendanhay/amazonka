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
-- Module      : Amazonka.EC2.ModifyLaunchTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies a launch template. You can specify which version of the launch
-- template to set as the default version. When launching an instance, the
-- default version applies when a launch template version is not specified.
module Amazonka.EC2.ModifyLaunchTemplate
  ( -- * Creating a Request
    ModifyLaunchTemplate (..),
    newModifyLaunchTemplate,

    -- * Request Lenses
    modifyLaunchTemplate_clientToken,
    modifyLaunchTemplate_defaultVersion,
    modifyLaunchTemplate_dryRun,
    modifyLaunchTemplate_launchTemplateId,
    modifyLaunchTemplate_launchTemplateName,

    -- * Destructuring the Response
    ModifyLaunchTemplateResponse (..),
    newModifyLaunchTemplateResponse,

    -- * Response Lenses
    modifyLaunchTemplateResponse_launchTemplate,
    modifyLaunchTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyLaunchTemplate' smart constructor.
data ModifyLaunchTemplate = ModifyLaunchTemplate'
  { -- | Unique, case-sensitive identifier you provide to ensure the idempotency
    -- of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
    --
    -- Constraint: Maximum 128 ASCII characters.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The version number of the launch template to set as the default version.
    defaultVersion :: Prelude.Maybe Prelude.Text,
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
    -- You must specify either the @LaunchTemplateName@ or the
    -- @LaunchTemplateId@, but not both.
    launchTemplateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyLaunchTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'modifyLaunchTemplate_clientToken' - Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
--
-- 'defaultVersion', 'modifyLaunchTemplate_defaultVersion' - The version number of the launch template to set as the default version.
--
-- 'dryRun', 'modifyLaunchTemplate_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'launchTemplateId', 'modifyLaunchTemplate_launchTemplateId' - The ID of the launch template.
--
-- You must specify either the @LaunchTemplateId@ or the
-- @LaunchTemplateName@, but not both.
--
-- 'launchTemplateName', 'modifyLaunchTemplate_launchTemplateName' - The name of the launch template.
--
-- You must specify either the @LaunchTemplateName@ or the
-- @LaunchTemplateId@, but not both.
newModifyLaunchTemplate ::
  ModifyLaunchTemplate
newModifyLaunchTemplate =
  ModifyLaunchTemplate'
    { clientToken =
        Prelude.Nothing,
      defaultVersion = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      launchTemplateId = Prelude.Nothing,
      launchTemplateName = Prelude.Nothing
    }

-- | Unique, case-sensitive identifier you provide to ensure the idempotency
-- of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>.
--
-- Constraint: Maximum 128 ASCII characters.
modifyLaunchTemplate_clientToken :: Lens.Lens' ModifyLaunchTemplate (Prelude.Maybe Prelude.Text)
modifyLaunchTemplate_clientToken = Lens.lens (\ModifyLaunchTemplate' {clientToken} -> clientToken) (\s@ModifyLaunchTemplate' {} a -> s {clientToken = a} :: ModifyLaunchTemplate)

-- | The version number of the launch template to set as the default version.
modifyLaunchTemplate_defaultVersion :: Lens.Lens' ModifyLaunchTemplate (Prelude.Maybe Prelude.Text)
modifyLaunchTemplate_defaultVersion = Lens.lens (\ModifyLaunchTemplate' {defaultVersion} -> defaultVersion) (\s@ModifyLaunchTemplate' {} a -> s {defaultVersion = a} :: ModifyLaunchTemplate)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyLaunchTemplate_dryRun :: Lens.Lens' ModifyLaunchTemplate (Prelude.Maybe Prelude.Bool)
modifyLaunchTemplate_dryRun = Lens.lens (\ModifyLaunchTemplate' {dryRun} -> dryRun) (\s@ModifyLaunchTemplate' {} a -> s {dryRun = a} :: ModifyLaunchTemplate)

-- | The ID of the launch template.
--
-- You must specify either the @LaunchTemplateId@ or the
-- @LaunchTemplateName@, but not both.
modifyLaunchTemplate_launchTemplateId :: Lens.Lens' ModifyLaunchTemplate (Prelude.Maybe Prelude.Text)
modifyLaunchTemplate_launchTemplateId = Lens.lens (\ModifyLaunchTemplate' {launchTemplateId} -> launchTemplateId) (\s@ModifyLaunchTemplate' {} a -> s {launchTemplateId = a} :: ModifyLaunchTemplate)

-- | The name of the launch template.
--
-- You must specify either the @LaunchTemplateName@ or the
-- @LaunchTemplateId@, but not both.
modifyLaunchTemplate_launchTemplateName :: Lens.Lens' ModifyLaunchTemplate (Prelude.Maybe Prelude.Text)
modifyLaunchTemplate_launchTemplateName = Lens.lens (\ModifyLaunchTemplate' {launchTemplateName} -> launchTemplateName) (\s@ModifyLaunchTemplate' {} a -> s {launchTemplateName = a} :: ModifyLaunchTemplate)

instance Core.AWSRequest ModifyLaunchTemplate where
  type
    AWSResponse ModifyLaunchTemplate =
      ModifyLaunchTemplateResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyLaunchTemplateResponse'
            Prelude.<$> (x Data..@? "launchTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyLaunchTemplate where
  hashWithSalt _salt ModifyLaunchTemplate' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` defaultVersion
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` launchTemplateId
      `Prelude.hashWithSalt` launchTemplateName

instance Prelude.NFData ModifyLaunchTemplate where
  rnf ModifyLaunchTemplate' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf defaultVersion
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf launchTemplateId
      `Prelude.seq` Prelude.rnf launchTemplateName

instance Data.ToHeaders ModifyLaunchTemplate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyLaunchTemplate where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyLaunchTemplate where
  toQuery ModifyLaunchTemplate' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyLaunchTemplate" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "SetDefaultVersion" Data.=: defaultVersion,
        "DryRun" Data.=: dryRun,
        "LaunchTemplateId" Data.=: launchTemplateId,
        "LaunchTemplateName" Data.=: launchTemplateName
      ]

-- | /See:/ 'newModifyLaunchTemplateResponse' smart constructor.
data ModifyLaunchTemplateResponse = ModifyLaunchTemplateResponse'
  { -- | Information about the launch template.
    launchTemplate :: Prelude.Maybe LaunchTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData ModifyLaunchTemplateResponse where
  rnf ModifyLaunchTemplateResponse' {..} =
    Prelude.rnf launchTemplate
      `Prelude.seq` Prelude.rnf httpStatus
