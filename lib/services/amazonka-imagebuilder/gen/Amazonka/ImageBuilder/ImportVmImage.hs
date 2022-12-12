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
-- Module      : Amazonka.ImageBuilder.ImportVmImage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- When you export your virtual machine (VM) from its virtualization
-- environment, that process creates a set of one or more disk container
-- files that act as snapshots of your VM’s environment, settings, and
-- data. The Amazon EC2 API
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_ImportImage.html ImportImage>
-- action uses those files to import your VM and create an AMI. To import
-- using the CLI command, see
-- <https://docs.aws.amazon.com/cli/latest/reference/ec2/import-image.html import-image>
--
-- You can reference the task ID from the VM import to pull in the AMI that
-- the import created as the base image for your Image Builder recipe.
module Amazonka.ImageBuilder.ImportVmImage
  ( -- * Creating a Request
    ImportVmImage (..),
    newImportVmImage,

    -- * Request Lenses
    importVmImage_description,
    importVmImage_osVersion,
    importVmImage_tags,
    importVmImage_name,
    importVmImage_semanticVersion,
    importVmImage_platform,
    importVmImage_vmImportTaskId,
    importVmImage_clientToken,

    -- * Destructuring the Response
    ImportVmImageResponse (..),
    newImportVmImageResponse,

    -- * Response Lenses
    importVmImageResponse_clientToken,
    importVmImageResponse_imageArn,
    importVmImageResponse_requestId,
    importVmImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportVmImage' smart constructor.
data ImportVmImage = ImportVmImage'
  { -- | The description for the base image that is created by the import
    -- process.
    description :: Prelude.Maybe Prelude.Text,
    -- | The operating system version for the imported VM.
    osVersion :: Prelude.Maybe Prelude.Text,
    -- | Tags that are attached to the import resources.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the base image that is created by the import process.
    name :: Prelude.Text,
    -- | The semantic version to attach to the base image that was created during
    -- the import process. This version follows the semantic version syntax.
    --
    -- The semantic version has four nodes:
    -- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
    -- first three, and can filter on all of them.
    --
    -- __Assignment:__ For the first three nodes you can assign any positive
    -- integer value, including zero, with an upper limit of 2^30-1, or
    -- 1073741823 for each node. Image Builder automatically assigns the build
    -- number to the fourth node.
    --
    -- __Patterns:__ You can use any numeric pattern that adheres to the
    -- assignment requirements for the nodes that you can assign. For example,
    -- you might choose a software version pattern, such as 1.0.0, or a date,
    -- such as 2021.01.01.
    semanticVersion :: Prelude.Text,
    -- | The operating system platform for the imported VM.
    platform :: Platform,
    -- | The @importTaskId@ (API) or @ImportTaskId@ (CLI) from the Amazon EC2 VM
    -- import process. Image Builder retrieves information from the import
    -- process to pull in the AMI that is created from the VM source as the
    -- base image for your recipe.
    vmImportTaskId :: Prelude.Text,
    -- | Unique, case-sensitive identifier you provide to ensure idempotency of
    -- the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>
    -- in the /Amazon EC2 API Reference/.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportVmImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'importVmImage_description' - The description for the base image that is created by the import
-- process.
--
-- 'osVersion', 'importVmImage_osVersion' - The operating system version for the imported VM.
--
-- 'tags', 'importVmImage_tags' - Tags that are attached to the import resources.
--
-- 'name', 'importVmImage_name' - The name of the base image that is created by the import process.
--
-- 'semanticVersion', 'importVmImage_semanticVersion' - The semantic version to attach to the base image that was created during
-- the import process. This version follows the semantic version syntax.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
--
-- 'platform', 'importVmImage_platform' - The operating system platform for the imported VM.
--
-- 'vmImportTaskId', 'importVmImage_vmImportTaskId' - The @importTaskId@ (API) or @ImportTaskId@ (CLI) from the Amazon EC2 VM
-- import process. Image Builder retrieves information from the import
-- process to pull in the AMI that is created from the VM source as the
-- base image for your recipe.
--
-- 'clientToken', 'importVmImage_clientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of
-- the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>
-- in the /Amazon EC2 API Reference/.
newImportVmImage ::
  -- | 'name'
  Prelude.Text ->
  -- | 'semanticVersion'
  Prelude.Text ->
  -- | 'platform'
  Platform ->
  -- | 'vmImportTaskId'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  ImportVmImage
newImportVmImage
  pName_
  pSemanticVersion_
  pPlatform_
  pVmImportTaskId_
  pClientToken_ =
    ImportVmImage'
      { description = Prelude.Nothing,
        osVersion = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        semanticVersion = pSemanticVersion_,
        platform = pPlatform_,
        vmImportTaskId = pVmImportTaskId_,
        clientToken = pClientToken_
      }

-- | The description for the base image that is created by the import
-- process.
importVmImage_description :: Lens.Lens' ImportVmImage (Prelude.Maybe Prelude.Text)
importVmImage_description = Lens.lens (\ImportVmImage' {description} -> description) (\s@ImportVmImage' {} a -> s {description = a} :: ImportVmImage)

-- | The operating system version for the imported VM.
importVmImage_osVersion :: Lens.Lens' ImportVmImage (Prelude.Maybe Prelude.Text)
importVmImage_osVersion = Lens.lens (\ImportVmImage' {osVersion} -> osVersion) (\s@ImportVmImage' {} a -> s {osVersion = a} :: ImportVmImage)

-- | Tags that are attached to the import resources.
importVmImage_tags :: Lens.Lens' ImportVmImage (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
importVmImage_tags = Lens.lens (\ImportVmImage' {tags} -> tags) (\s@ImportVmImage' {} a -> s {tags = a} :: ImportVmImage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the base image that is created by the import process.
importVmImage_name :: Lens.Lens' ImportVmImage Prelude.Text
importVmImage_name = Lens.lens (\ImportVmImage' {name} -> name) (\s@ImportVmImage' {} a -> s {name = a} :: ImportVmImage)

-- | The semantic version to attach to the base image that was created during
-- the import process. This version follows the semantic version syntax.
--
-- The semantic version has four nodes:
-- \<major>.\<minor>.\<patch>\/\<build>. You can assign values for the
-- first three, and can filter on all of them.
--
-- __Assignment:__ For the first three nodes you can assign any positive
-- integer value, including zero, with an upper limit of 2^30-1, or
-- 1073741823 for each node. Image Builder automatically assigns the build
-- number to the fourth node.
--
-- __Patterns:__ You can use any numeric pattern that adheres to the
-- assignment requirements for the nodes that you can assign. For example,
-- you might choose a software version pattern, such as 1.0.0, or a date,
-- such as 2021.01.01.
importVmImage_semanticVersion :: Lens.Lens' ImportVmImage Prelude.Text
importVmImage_semanticVersion = Lens.lens (\ImportVmImage' {semanticVersion} -> semanticVersion) (\s@ImportVmImage' {} a -> s {semanticVersion = a} :: ImportVmImage)

-- | The operating system platform for the imported VM.
importVmImage_platform :: Lens.Lens' ImportVmImage Platform
importVmImage_platform = Lens.lens (\ImportVmImage' {platform} -> platform) (\s@ImportVmImage' {} a -> s {platform = a} :: ImportVmImage)

-- | The @importTaskId@ (API) or @ImportTaskId@ (CLI) from the Amazon EC2 VM
-- import process. Image Builder retrieves information from the import
-- process to pull in the AMI that is created from the VM source as the
-- base image for your recipe.
importVmImage_vmImportTaskId :: Lens.Lens' ImportVmImage Prelude.Text
importVmImage_vmImportTaskId = Lens.lens (\ImportVmImage' {vmImportTaskId} -> vmImportTaskId) (\s@ImportVmImage' {} a -> s {vmImportTaskId = a} :: ImportVmImage)

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring idempotency>
-- in the /Amazon EC2 API Reference/.
importVmImage_clientToken :: Lens.Lens' ImportVmImage Prelude.Text
importVmImage_clientToken = Lens.lens (\ImportVmImage' {clientToken} -> clientToken) (\s@ImportVmImage' {} a -> s {clientToken = a} :: ImportVmImage)

instance Core.AWSRequest ImportVmImage where
  type
    AWSResponse ImportVmImage =
      ImportVmImageResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportVmImageResponse'
            Prelude.<$> (x Data..?> "clientToken")
            Prelude.<*> (x Data..?> "imageArn")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportVmImage where
  hashWithSalt _salt ImportVmImage' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` osVersion
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` semanticVersion
      `Prelude.hashWithSalt` platform
      `Prelude.hashWithSalt` vmImportTaskId
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData ImportVmImage where
  rnf ImportVmImage' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf osVersion
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf platform
      `Prelude.seq` Prelude.rnf vmImportTaskId
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders ImportVmImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportVmImage where
  toJSON ImportVmImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("osVersion" Data..=) Prelude.<$> osVersion,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("semanticVersion" Data..= semanticVersion),
            Prelude.Just ("platform" Data..= platform),
            Prelude.Just
              ("vmImportTaskId" Data..= vmImportTaskId),
            Prelude.Just ("clientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath ImportVmImage where
  toPath = Prelude.const "/ImportVmImage"

instance Data.ToQuery ImportVmImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportVmImageResponse' smart constructor.
data ImportVmImageResponse = ImportVmImageResponse'
  { -- | The idempotency token that was used for this request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the AMI that was created during the VM
    -- import process. This AMI is used as the base image for the recipe that
    -- imported the VM.
    imageArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportVmImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'importVmImageResponse_clientToken' - The idempotency token that was used for this request.
--
-- 'imageArn', 'importVmImageResponse_imageArn' - The Amazon Resource Name (ARN) of the AMI that was created during the VM
-- import process. This AMI is used as the base image for the recipe that
-- imported the VM.
--
-- 'requestId', 'importVmImageResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'importVmImageResponse_httpStatus' - The response's http status code.
newImportVmImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportVmImageResponse
newImportVmImageResponse pHttpStatus_ =
  ImportVmImageResponse'
    { clientToken =
        Prelude.Nothing,
      imageArn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The idempotency token that was used for this request.
importVmImageResponse_clientToken :: Lens.Lens' ImportVmImageResponse (Prelude.Maybe Prelude.Text)
importVmImageResponse_clientToken = Lens.lens (\ImportVmImageResponse' {clientToken} -> clientToken) (\s@ImportVmImageResponse' {} a -> s {clientToken = a} :: ImportVmImageResponse)

-- | The Amazon Resource Name (ARN) of the AMI that was created during the VM
-- import process. This AMI is used as the base image for the recipe that
-- imported the VM.
importVmImageResponse_imageArn :: Lens.Lens' ImportVmImageResponse (Prelude.Maybe Prelude.Text)
importVmImageResponse_imageArn = Lens.lens (\ImportVmImageResponse' {imageArn} -> imageArn) (\s@ImportVmImageResponse' {} a -> s {imageArn = a} :: ImportVmImageResponse)

-- | The request ID that uniquely identifies this request.
importVmImageResponse_requestId :: Lens.Lens' ImportVmImageResponse (Prelude.Maybe Prelude.Text)
importVmImageResponse_requestId = Lens.lens (\ImportVmImageResponse' {requestId} -> requestId) (\s@ImportVmImageResponse' {} a -> s {requestId = a} :: ImportVmImageResponse)

-- | The response's http status code.
importVmImageResponse_httpStatus :: Lens.Lens' ImportVmImageResponse Prelude.Int
importVmImageResponse_httpStatus = Lens.lens (\ImportVmImageResponse' {httpStatus} -> httpStatus) (\s@ImportVmImageResponse' {} a -> s {httpStatus = a} :: ImportVmImageResponse)

instance Prelude.NFData ImportVmImageResponse where
  rnf ImportVmImageResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf imageArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
