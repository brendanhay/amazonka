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
-- Module      : Amazonka.DrS.CreateLaunchConfigurationTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Launch Configuration Template.
module Amazonka.DrS.CreateLaunchConfigurationTemplate
  ( -- * Creating a Request
    CreateLaunchConfigurationTemplate (..),
    newCreateLaunchConfigurationTemplate,

    -- * Request Lenses
    createLaunchConfigurationTemplate_copyPrivateIp,
    createLaunchConfigurationTemplate_copyTags,
    createLaunchConfigurationTemplate_exportBucketArn,
    createLaunchConfigurationTemplate_launchDisposition,
    createLaunchConfigurationTemplate_licensing,
    createLaunchConfigurationTemplate_tags,
    createLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod,

    -- * Destructuring the Response
    CreateLaunchConfigurationTemplateResponse (..),
    newCreateLaunchConfigurationTemplateResponse,

    -- * Response Lenses
    createLaunchConfigurationTemplateResponse_launchConfigurationTemplate,
    createLaunchConfigurationTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLaunchConfigurationTemplate' smart constructor.
data CreateLaunchConfigurationTemplate = CreateLaunchConfigurationTemplate'
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
    -- | Request to associate tags during creation of a Launch Configuration
    -- Template.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Target instance type right-sizing method.
    targetInstanceTypeRightSizingMethod :: Prelude.Maybe TargetInstanceTypeRightSizingMethod
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchConfigurationTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'copyPrivateIp', 'createLaunchConfigurationTemplate_copyPrivateIp' - Copy private IP.
--
-- 'copyTags', 'createLaunchConfigurationTemplate_copyTags' - Copy tags.
--
-- 'exportBucketArn', 'createLaunchConfigurationTemplate_exportBucketArn' - S3 bucket ARN to export Source Network templates.
--
-- 'launchDisposition', 'createLaunchConfigurationTemplate_launchDisposition' - Launch disposition.
--
-- 'licensing', 'createLaunchConfigurationTemplate_licensing' - Licensing.
--
-- 'tags', 'createLaunchConfigurationTemplate_tags' - Request to associate tags during creation of a Launch Configuration
-- Template.
--
-- 'targetInstanceTypeRightSizingMethod', 'createLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod' - Target instance type right-sizing method.
newCreateLaunchConfigurationTemplate ::
  CreateLaunchConfigurationTemplate
newCreateLaunchConfigurationTemplate =
  CreateLaunchConfigurationTemplate'
    { copyPrivateIp =
        Prelude.Nothing,
      copyTags = Prelude.Nothing,
      exportBucketArn = Prelude.Nothing,
      launchDisposition = Prelude.Nothing,
      licensing = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetInstanceTypeRightSizingMethod =
        Prelude.Nothing
    }

-- | Copy private IP.
createLaunchConfigurationTemplate_copyPrivateIp :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
createLaunchConfigurationTemplate_copyPrivateIp = Lens.lens (\CreateLaunchConfigurationTemplate' {copyPrivateIp} -> copyPrivateIp) (\s@CreateLaunchConfigurationTemplate' {} a -> s {copyPrivateIp = a} :: CreateLaunchConfigurationTemplate)

-- | Copy tags.
createLaunchConfigurationTemplate_copyTags :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Bool)
createLaunchConfigurationTemplate_copyTags = Lens.lens (\CreateLaunchConfigurationTemplate' {copyTags} -> copyTags) (\s@CreateLaunchConfigurationTemplate' {} a -> s {copyTags = a} :: CreateLaunchConfigurationTemplate)

-- | S3 bucket ARN to export Source Network templates.
createLaunchConfigurationTemplate_exportBucketArn :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Prelude.Text)
createLaunchConfigurationTemplate_exportBucketArn = Lens.lens (\CreateLaunchConfigurationTemplate' {exportBucketArn} -> exportBucketArn) (\s@CreateLaunchConfigurationTemplate' {} a -> s {exportBucketArn = a} :: CreateLaunchConfigurationTemplate)

-- | Launch disposition.
createLaunchConfigurationTemplate_launchDisposition :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe LaunchDisposition)
createLaunchConfigurationTemplate_launchDisposition = Lens.lens (\CreateLaunchConfigurationTemplate' {launchDisposition} -> launchDisposition) (\s@CreateLaunchConfigurationTemplate' {} a -> s {launchDisposition = a} :: CreateLaunchConfigurationTemplate)

-- | Licensing.
createLaunchConfigurationTemplate_licensing :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe Licensing)
createLaunchConfigurationTemplate_licensing = Lens.lens (\CreateLaunchConfigurationTemplate' {licensing} -> licensing) (\s@CreateLaunchConfigurationTemplate' {} a -> s {licensing = a} :: CreateLaunchConfigurationTemplate)

-- | Request to associate tags during creation of a Launch Configuration
-- Template.
createLaunchConfigurationTemplate_tags :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLaunchConfigurationTemplate_tags = Lens.lens (\CreateLaunchConfigurationTemplate' {tags} -> tags) (\s@CreateLaunchConfigurationTemplate' {} a -> s {tags = a} :: CreateLaunchConfigurationTemplate) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Target instance type right-sizing method.
createLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod :: Lens.Lens' CreateLaunchConfigurationTemplate (Prelude.Maybe TargetInstanceTypeRightSizingMethod)
createLaunchConfigurationTemplate_targetInstanceTypeRightSizingMethod = Lens.lens (\CreateLaunchConfigurationTemplate' {targetInstanceTypeRightSizingMethod} -> targetInstanceTypeRightSizingMethod) (\s@CreateLaunchConfigurationTemplate' {} a -> s {targetInstanceTypeRightSizingMethod = a} :: CreateLaunchConfigurationTemplate)

instance
  Core.AWSRequest
    CreateLaunchConfigurationTemplate
  where
  type
    AWSResponse CreateLaunchConfigurationTemplate =
      CreateLaunchConfigurationTemplateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLaunchConfigurationTemplateResponse'
            Prelude.<$> (x Data..?> "launchConfigurationTemplate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateLaunchConfigurationTemplate
  where
  hashWithSalt
    _salt
    CreateLaunchConfigurationTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` copyPrivateIp
        `Prelude.hashWithSalt` copyTags
        `Prelude.hashWithSalt` exportBucketArn
        `Prelude.hashWithSalt` launchDisposition
        `Prelude.hashWithSalt` licensing
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` targetInstanceTypeRightSizingMethod

instance
  Prelude.NFData
    CreateLaunchConfigurationTemplate
  where
  rnf CreateLaunchConfigurationTemplate' {..} =
    Prelude.rnf copyPrivateIp
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf exportBucketArn
      `Prelude.seq` Prelude.rnf launchDisposition
      `Prelude.seq` Prelude.rnf licensing
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetInstanceTypeRightSizingMethod

instance
  Data.ToHeaders
    CreateLaunchConfigurationTemplate
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
    CreateLaunchConfigurationTemplate
  where
  toJSON CreateLaunchConfigurationTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("copyPrivateIp" Data..=) Prelude.<$> copyPrivateIp,
            ("copyTags" Data..=) Prelude.<$> copyTags,
            ("exportBucketArn" Data..=)
              Prelude.<$> exportBucketArn,
            ("launchDisposition" Data..=)
              Prelude.<$> launchDisposition,
            ("licensing" Data..=) Prelude.<$> licensing,
            ("tags" Data..=) Prelude.<$> tags,
            ("targetInstanceTypeRightSizingMethod" Data..=)
              Prelude.<$> targetInstanceTypeRightSizingMethod
          ]
      )

instance
  Data.ToPath
    CreateLaunchConfigurationTemplate
  where
  toPath =
    Prelude.const "/CreateLaunchConfigurationTemplate"

instance
  Data.ToQuery
    CreateLaunchConfigurationTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLaunchConfigurationTemplateResponse' smart constructor.
data CreateLaunchConfigurationTemplateResponse = CreateLaunchConfigurationTemplateResponse'
  { -- | Created Launch Configuration Template.
    launchConfigurationTemplate :: Prelude.Maybe LaunchConfigurationTemplate,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLaunchConfigurationTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'launchConfigurationTemplate', 'createLaunchConfigurationTemplateResponse_launchConfigurationTemplate' - Created Launch Configuration Template.
--
-- 'httpStatus', 'createLaunchConfigurationTemplateResponse_httpStatus' - The response's http status code.
newCreateLaunchConfigurationTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLaunchConfigurationTemplateResponse
newCreateLaunchConfigurationTemplateResponse
  pHttpStatus_ =
    CreateLaunchConfigurationTemplateResponse'
      { launchConfigurationTemplate =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Created Launch Configuration Template.
createLaunchConfigurationTemplateResponse_launchConfigurationTemplate :: Lens.Lens' CreateLaunchConfigurationTemplateResponse (Prelude.Maybe LaunchConfigurationTemplate)
createLaunchConfigurationTemplateResponse_launchConfigurationTemplate = Lens.lens (\CreateLaunchConfigurationTemplateResponse' {launchConfigurationTemplate} -> launchConfigurationTemplate) (\s@CreateLaunchConfigurationTemplateResponse' {} a -> s {launchConfigurationTemplate = a} :: CreateLaunchConfigurationTemplateResponse)

-- | The response's http status code.
createLaunchConfigurationTemplateResponse_httpStatus :: Lens.Lens' CreateLaunchConfigurationTemplateResponse Prelude.Int
createLaunchConfigurationTemplateResponse_httpStatus = Lens.lens (\CreateLaunchConfigurationTemplateResponse' {httpStatus} -> httpStatus) (\s@CreateLaunchConfigurationTemplateResponse' {} a -> s {httpStatus = a} :: CreateLaunchConfigurationTemplateResponse)

instance
  Prelude.NFData
    CreateLaunchConfigurationTemplateResponse
  where
  rnf CreateLaunchConfigurationTemplateResponse' {..} =
    Prelude.rnf launchConfigurationTemplate
      `Prelude.seq` Prelude.rnf httpStatus
