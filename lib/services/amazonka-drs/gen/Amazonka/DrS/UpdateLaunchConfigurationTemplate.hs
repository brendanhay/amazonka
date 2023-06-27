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
-- Module      : Amazonka.DrS.UpdateLaunchConfigurationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Launch Configuration Template by ID.
module Amazonka.DrS.UpdateLaunchConfigurationTemplate
  ( -- * Creating a Request
    UpdateLaunchConfigurationTemplate (..),
    newUpdateLaunchConfigurationTemplate,

    -- * Request Lenses
    updateLaunchConfigurationTemplate_copyPrivateIp,
    updateLaunchConfigurationTemplate_copyTags,
    updateLaunchConfigurationTemplate_exportBucketArn,
    updateLaunchConfigurationTemplate_launchDisposition,
    updateLaunchConfigurationTemplate_licensing,
    updateLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod,
    updateLaunchConfigurationTemplate_launchConfigurationTemplateID,

    -- * Destructuring the Response
    UpdateLaunchConfigurationTemplateResponse (..),
    newUpdateLaunchConfigurationTemplateResponse,

    -- * Response Lenses
    updateLaunchConfigurationTemplateResponse_launchConfigurationTemplate,
    updateLaunchConfigurationTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLaunchConfigurationTemplate' smart constructor.
data UpdateLaunchConfigurationTemplate = UpdateLaunchConfigurationTemplate'
  { -- | Copy private IP.
    copyPrivateIp :: Prelude.Maybe Prelude.Bool,
    -- | Copy tags.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | S3 bucket ARN to export Source Network templates.
    exportBucketArn :: Prelude.Maybe Prelude.Text,
    -- | Launch disposition.
    launchDisposition :: Prelude.Maybe LaunchDisposition,
    -- | Licensing.
    licensing :: Prelude.Maybe Licensing,
    -- | Target instance type right-sizing method.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod,
    -- | Launch Configuration Template ID.
    launchConfigurationTemplateID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLaunchConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyPrivateIp', 'updateLaunchConfigurationTemplate_copyPrivateIp' - Copy private IP.
--
-- 'copyTags', 'updateLaunchConfigurationTemplate_copyTags' - Copy tags.
--
-- 'exportBucketArn', 'updateLaunchConfigurationTemplate_exportBucketArn' - S3 bucket ARN to export Source Network templates.
--
-- 'launchDisposition', 'updateLaunchConfigurationTemplate_launchDisposition' - Launch disposition.
--
-- 'licensing', 'updateLaunchConfigurationTemplate_licensing' - Licensing.
--
-- 'targetInstanceTypeRightSizingMethod', 'updateLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod' - Target instance type right-sizing method.
--
-- 'launchConfigurationTemplateID', 'updateLaunchConfigurationTemplate_launchConfigurationTemplateID' - Launch Configuration Template ID.
newUpdateLaunchConfigurationTemplate ::
  -- | 'launchConfigurationTemplateID'
  Prelude.Text ->
  UpdateLaunchConfigurationTemplate
newUpdateLaunchConfigurationTemplate
  pLaunchConfigurationTemplateID_ =
    UpdateLaunchConfigurationTemplate'
      { copyPrivateIp =
          Prelude.Nothing,
        copyTags = Prelude.Nothing,
        exportBucketArn = Prelude.Nothing,
        launchDisposition = Prelude.Nothing,
        licensing = Prelude.Nothing,
        targetInstanceTypeRightSizingMethod =
          Prelude.Nothing,
        launchConfigurationTemplateID =
          pLaunchConfigurationTemplateID_
      }

-- | Copy private IP.
updateLaunchConfigurationTemplate_copyPrivateIp :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateLaunchConfigurationTemplate_copyPrivateIp = Lens.lens (\UpdateLaunchConfigurationTemplate' {copyPrivateIp} -> copyPrivateIp) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {copyPrivateIp = a} :: UpdateLaunchConfigurationTemplate)

-- | Copy tags.
updateLaunchConfigurationTemplate_copyTags :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
updateLaunchConfigurationTemplate_copyTags = Lens.lens (\UpdateLaunchConfigurationTemplate' {copyTags} -> copyTags) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {copyTags = a} :: UpdateLaunchConfigurationTemplate)

-- | S3 bucket ARN to export Source Network templates.
updateLaunchConfigurationTemplate_exportBucketArn :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
updateLaunchConfigurationTemplate_exportBucketArn = Lens.lens (\UpdateLaunchConfigurationTemplate' {exportBucketArn} -> exportBucketArn) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {exportBucketArn = a} :: UpdateLaunchConfigurationTemplate)

-- | Launch disposition.
updateLaunchConfigurationTemplate_launchDisposition :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe LaunchDisposition)
updateLaunchConfigurationTemplate_launchDisposition = Lens.lens (\UpdateLaunchConfigurationTemplate' {launchDisposition} -> launchDisposition) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {launchDisposition = a} :: UpdateLaunchConfigurationTemplate)

-- | Licensing.
updateLaunchConfigurationTemplate_licensing :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe Licensing)
updateLaunchConfigurationTemplate_licensing = Lens.lens (\UpdateLaunchConfigurationTemplate' {licensing} -> licensing) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {licensing = a} :: UpdateLaunchConfigurationTemplate)

-- | Target instance type right-sizing method.
updateLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod :: Lens.Lens' UpdateLaunchConfigurationTemplate (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
updateLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod = Lens.lens (\UpdateLaunchConfigurationTemplate' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: UpdateLaunchConfigurationTemplate)

-- | Launch Configuration Template ID.
updateLaunchConfigurationTemplate_launchConfigurationTemplateID :: Lens.Lens' UpdateLaunchConfigurationTemplate Prelude.Text
updateLaunchConfigurationTemplate_launchConfigurationTemplateID = Lens.lens (\UpdateLaunchConfigurationTemplate' {launchConfigurationTemplateID} -> launchConfigurationTemplateID) (\s@UpdateLaunchConfigurationTemplate' {} a -> s {launchConfigurationTemplateID = a} :: UpdateLaunchConfigurationTemplate)

instance
  Core.AWSRequest
    UpdateLaunchConfigurationTemplate
  where
  type
    AWSResponse UpdateLaunchConfigurationTemplate =
      UpdateLaunchConfigurationTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLaunchConfigurationTemplateResponse'
            Prelude.<$> (x Data..?> "launchConfigurationTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateLaunchConfigurationTemplate
  where
  hashWithSalt
    _salt
    UpdateLaunchConfigurationTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` copyPrivateIp
        `Prelude.hashWithSalt` copyTags
        `Prelude.hashWithSalt` exportBucketArn
        `Prelude.hashWithSalt` launchDisposition
        `Prelude.hashWithSalt` licensing
        `Prelude.hashWithSalt` targetInstanceTypeRightSizingMethod
        `Prelude.hashWithSalt` launchConfigurationTemplateID

instance
  Prelude.NFData
    UpdateLaunchConfigurationTemplate
  where
  rnf UpdateLaunchConfigurationTemplate' {..} =
    Prelude.rnf copyPrivateIp
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf exportBucketArn
      `Prelude.seq` Prelude.rnf launchDisposition
      `Prelude.seq` Prelude.rnf licensing
      `Prelude.seq` Prelude.rnf targetInstanceTypeRightSizingMethod
      `Prelude.seq` Prelude.rnf launchConfigurationTemplateID

instance
  Data.ToHeaders
    UpdateLaunchConfigurationTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateLaunchConfigurationTemplate
  where
  toJSON UpdateLaunchConfigurationTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("copyPrivateIp" Data..=) Prelude.<$> copyPrivateIp,
            ("copyTags" Data..=) Prelude.<$> copyTags,
            ("exportBucketArn" Data..=)
              Prelude.<$> exportBucketArn,
            ("launchDisposition" Data..=)
              Prelude.<$> launchDisposition,
            ("licensing" Data..=) Prelude.<$> licensing,
            ("targetInstanceTypeRightSizingMethod" Data..=)
              Prelude.<$> targetInstanceTypeRightSizingMethod,
            Prelude.Just
              ( "launchConfigurationTemplateID"
                  Data..= launchConfigurationTemplateID
              )
          ]
      )

instance
  Data.ToPath
    UpdateLaunchConfigurationTemplate
  where
  toPath =
    Prelude.const "/UpdateLaunchConfigurationTemplate"

instance
  Data.ToQuery
    UpdateLaunchConfigurationTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLaunchConfigurationTemplateResponse' smart constructor.
data UpdateLaunchConfigurationTemplateResponse = UpdateLaunchConfigurationTemplateResponse'
  { -- | Updated Launch Configuration Template.
    launchConfigurationTemplate :: Prelude.Maybe LaunchConfigurationTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLaunchConfigurationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchConfigurationTemplate', 'updateLaunchConfigurationTemplateResponse_launchConfigurationTemplate' - Updated Launch Configuration Template.
--
-- 'httpStatus', 'updateLaunchConfigurationTemplateResponse_httpStatus' - The response's http status code.
newUpdateLaunchConfigurationTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLaunchConfigurationTemplateResponse
newUpdateLaunchConfigurationTemplateResponse
  pHttpStatus_ =
    UpdateLaunchConfigurationTemplateResponse'
      { launchConfigurationTemplate =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Updated Launch Configuration Template.
updateLaunchConfigurationTemplateResponse_launchConfigurationTemplate :: Lens.Lens' UpdateLaunchConfigurationTemplateResponse (Prelude.Maybe LaunchConfigurationTemplate)
updateLaunchConfigurationTemplateResponse_launchConfigurationTemplate = Lens.lens (\UpdateLaunchConfigurationTemplateResponse' {launchConfigurationTemplate} -> launchConfigurationTemplate) (\s@UpdateLaunchConfigurationTemplateResponse' {} a -> s {launchConfigurationTemplate = a} :: UpdateLaunchConfigurationTemplateResponse)

-- | The response's http status code.
updateLaunchConfigurationTemplateResponse_httpStatus :: Lens.Lens' UpdateLaunchConfigurationTemplateResponse Prelude.Int
updateLaunchConfigurationTemplateResponse_httpStatus = Lens.lens (\UpdateLaunchConfigurationTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateLaunchConfigurationTemplateResponse' {} a -> s {httpStatus = a} :: UpdateLaunchConfigurationTemplateResponse)

instance
  Prelude.NFData
    UpdateLaunchConfigurationTemplateResponse
  where
  rnf UpdateLaunchConfigurationTemplateResponse' {..} =
    Prelude.rnf launchConfigurationTemplate
      `Prelude.seq` Prelude.rnf httpStatus
