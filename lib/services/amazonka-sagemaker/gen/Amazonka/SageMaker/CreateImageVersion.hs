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
-- Module      : Amazonka.SageMaker.CreateImageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a version of the SageMaker image specified by @ImageName@. The
-- version represents the Amazon Elastic Container Registry (ECR) container
-- image specified by @BaseImage@.
module Amazonka.SageMaker.CreateImageVersion
  ( -- * Creating a Request
    CreateImageVersion (..),
    newCreateImageVersion,

    -- * Request Lenses
    createImageVersion_aliases,
    createImageVersion_horovod,
    createImageVersion_jobType,
    createImageVersion_mLFramework,
    createImageVersion_processor,
    createImageVersion_programmingLang,
    createImageVersion_releaseNotes,
    createImageVersion_vendorGuidance,
    createImageVersion_baseImage,
    createImageVersion_clientToken,
    createImageVersion_imageName,

    -- * Destructuring the Response
    CreateImageVersionResponse (..),
    newCreateImageVersionResponse,

    -- * Response Lenses
    createImageVersionResponse_imageVersionArn,
    createImageVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateImageVersion' smart constructor.
data CreateImageVersion = CreateImageVersion'
  { -- | A list of aliases created with the image version.
    aliases :: Prelude.Maybe [Prelude.Text],
    -- | Indicates Horovod compatibility.
    horovod :: Prelude.Maybe Prelude.Bool,
    -- | Indicates SageMaker job type compatibility.
    --
    -- -   @TRAINING@: The image version is compatible with SageMaker training
    --     jobs.
    --
    -- -   @INFERENCE@: The image version is compatible with SageMaker
    --     inference jobs.
    --
    -- -   @NOTEBOOK_KERNEL@: The image version is compatible with SageMaker
    --     notebook kernels.
    jobType :: Prelude.Maybe JobType,
    -- | The machine learning framework vended in the image version.
    mLFramework :: Prelude.Maybe Prelude.Text,
    -- | Indicates CPU or GPU compatibility.
    --
    -- -   @CPU@: The image version is compatible with CPU.
    --
    -- -   @GPU@: The image version is compatible with GPU.
    processor :: Prelude.Maybe Processor,
    -- | The supported programming language and its version.
    programmingLang :: Prelude.Maybe Prelude.Text,
    -- | The maintainer description of the image version.
    releaseNotes :: Prelude.Maybe Prelude.Text,
    -- | The stability of the image version, specified by the maintainer.
    --
    -- -   @NOT_PROVIDED@: The maintainers did not provide a status for image
    --     version stability.
    --
    -- -   @STABLE@: The image version is stable.
    --
    -- -   @TO_BE_ARCHIVED@: The image version is set to be archived. Custom
    --     image versions that are set to be archived are automatically
    --     archived after three months.
    --
    -- -   @ARCHIVED@: The image version is archived. Archived image versions
    --     are not searchable and are no longer actively supported.
    vendorGuidance :: Prelude.Maybe VendorGuidance,
    -- | The registry path of the container image to use as the starting point
    -- for this version. The path is an Amazon Elastic Container Registry (ECR)
    -- URI in the following format:
    --
    -- @\<acct-id>.dkr.ecr.\<region>.amazonaws.com\/\<repo-name[:tag] or [\@digest]>@
    baseImage :: Prelude.Text,
    -- | A unique ID. If not specified, the Amazon Web Services CLI and Amazon
    -- Web Services SDKs, such as the SDK for Python (Boto3), add a unique
    -- value to the call.
    clientToken :: Prelude.Text,
    -- | The @ImageName@ of the @Image@ to create a version of.
    imageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliases', 'createImageVersion_aliases' - A list of aliases created with the image version.
--
-- 'horovod', 'createImageVersion_horovod' - Indicates Horovod compatibility.
--
-- 'jobType', 'createImageVersion_jobType' - Indicates SageMaker job type compatibility.
--
-- -   @TRAINING@: The image version is compatible with SageMaker training
--     jobs.
--
-- -   @INFERENCE@: The image version is compatible with SageMaker
--     inference jobs.
--
-- -   @NOTEBOOK_KERNEL@: The image version is compatible with SageMaker
--     notebook kernels.
--
-- 'mLFramework', 'createImageVersion_mLFramework' - The machine learning framework vended in the image version.
--
-- 'processor', 'createImageVersion_processor' - Indicates CPU or GPU compatibility.
--
-- -   @CPU@: The image version is compatible with CPU.
--
-- -   @GPU@: The image version is compatible with GPU.
--
-- 'programmingLang', 'createImageVersion_programmingLang' - The supported programming language and its version.
--
-- 'releaseNotes', 'createImageVersion_releaseNotes' - The maintainer description of the image version.
--
-- 'vendorGuidance', 'createImageVersion_vendorGuidance' - The stability of the image version, specified by the maintainer.
--
-- -   @NOT_PROVIDED@: The maintainers did not provide a status for image
--     version stability.
--
-- -   @STABLE@: The image version is stable.
--
-- -   @TO_BE_ARCHIVED@: The image version is set to be archived. Custom
--     image versions that are set to be archived are automatically
--     archived after three months.
--
-- -   @ARCHIVED@: The image version is archived. Archived image versions
--     are not searchable and are no longer actively supported.
--
-- 'baseImage', 'createImageVersion_baseImage' - The registry path of the container image to use as the starting point
-- for this version. The path is an Amazon Elastic Container Registry (ECR)
-- URI in the following format:
--
-- @\<acct-id>.dkr.ecr.\<region>.amazonaws.com\/\<repo-name[:tag] or [\@digest]>@
--
-- 'clientToken', 'createImageVersion_clientToken' - A unique ID. If not specified, the Amazon Web Services CLI and Amazon
-- Web Services SDKs, such as the SDK for Python (Boto3), add a unique
-- value to the call.
--
-- 'imageName', 'createImageVersion_imageName' - The @ImageName@ of the @Image@ to create a version of.
newCreateImageVersion ::
  -- | 'baseImage'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'imageName'
  Prelude.Text ->
  CreateImageVersion
newCreateImageVersion
  pBaseImage_
  pClientToken_
  pImageName_ =
    CreateImageVersion'
      { aliases = Prelude.Nothing,
        horovod = Prelude.Nothing,
        jobType = Prelude.Nothing,
        mLFramework = Prelude.Nothing,
        processor = Prelude.Nothing,
        programmingLang = Prelude.Nothing,
        releaseNotes = Prelude.Nothing,
        vendorGuidance = Prelude.Nothing,
        baseImage = pBaseImage_,
        clientToken = pClientToken_,
        imageName = pImageName_
      }

-- | A list of aliases created with the image version.
createImageVersion_aliases :: Lens.Lens' CreateImageVersion (Prelude.Maybe [Prelude.Text])
createImageVersion_aliases = Lens.lens (\CreateImageVersion' {aliases} -> aliases) (\s@CreateImageVersion' {} a -> s {aliases = a} :: CreateImageVersion) Prelude.. Lens.mapping Lens.coerced

-- | Indicates Horovod compatibility.
createImageVersion_horovod :: Lens.Lens' CreateImageVersion (Prelude.Maybe Prelude.Bool)
createImageVersion_horovod = Lens.lens (\CreateImageVersion' {horovod} -> horovod) (\s@CreateImageVersion' {} a -> s {horovod = a} :: CreateImageVersion)

-- | Indicates SageMaker job type compatibility.
--
-- -   @TRAINING@: The image version is compatible with SageMaker training
--     jobs.
--
-- -   @INFERENCE@: The image version is compatible with SageMaker
--     inference jobs.
--
-- -   @NOTEBOOK_KERNEL@: The image version is compatible with SageMaker
--     notebook kernels.
createImageVersion_jobType :: Lens.Lens' CreateImageVersion (Prelude.Maybe JobType)
createImageVersion_jobType = Lens.lens (\CreateImageVersion' {jobType} -> jobType) (\s@CreateImageVersion' {} a -> s {jobType = a} :: CreateImageVersion)

-- | The machine learning framework vended in the image version.
createImageVersion_mLFramework :: Lens.Lens' CreateImageVersion (Prelude.Maybe Prelude.Text)
createImageVersion_mLFramework = Lens.lens (\CreateImageVersion' {mLFramework} -> mLFramework) (\s@CreateImageVersion' {} a -> s {mLFramework = a} :: CreateImageVersion)

-- | Indicates CPU or GPU compatibility.
--
-- -   @CPU@: The image version is compatible with CPU.
--
-- -   @GPU@: The image version is compatible with GPU.
createImageVersion_processor :: Lens.Lens' CreateImageVersion (Prelude.Maybe Processor)
createImageVersion_processor = Lens.lens (\CreateImageVersion' {processor} -> processor) (\s@CreateImageVersion' {} a -> s {processor = a} :: CreateImageVersion)

-- | The supported programming language and its version.
createImageVersion_programmingLang :: Lens.Lens' CreateImageVersion (Prelude.Maybe Prelude.Text)
createImageVersion_programmingLang = Lens.lens (\CreateImageVersion' {programmingLang} -> programmingLang) (\s@CreateImageVersion' {} a -> s {programmingLang = a} :: CreateImageVersion)

-- | The maintainer description of the image version.
createImageVersion_releaseNotes :: Lens.Lens' CreateImageVersion (Prelude.Maybe Prelude.Text)
createImageVersion_releaseNotes = Lens.lens (\CreateImageVersion' {releaseNotes} -> releaseNotes) (\s@CreateImageVersion' {} a -> s {releaseNotes = a} :: CreateImageVersion)

-- | The stability of the image version, specified by the maintainer.
--
-- -   @NOT_PROVIDED@: The maintainers did not provide a status for image
--     version stability.
--
-- -   @STABLE@: The image version is stable.
--
-- -   @TO_BE_ARCHIVED@: The image version is set to be archived. Custom
--     image versions that are set to be archived are automatically
--     archived after three months.
--
-- -   @ARCHIVED@: The image version is archived. Archived image versions
--     are not searchable and are no longer actively supported.
createImageVersion_vendorGuidance :: Lens.Lens' CreateImageVersion (Prelude.Maybe VendorGuidance)
createImageVersion_vendorGuidance = Lens.lens (\CreateImageVersion' {vendorGuidance} -> vendorGuidance) (\s@CreateImageVersion' {} a -> s {vendorGuidance = a} :: CreateImageVersion)

-- | The registry path of the container image to use as the starting point
-- for this version. The path is an Amazon Elastic Container Registry (ECR)
-- URI in the following format:
--
-- @\<acct-id>.dkr.ecr.\<region>.amazonaws.com\/\<repo-name[:tag] or [\@digest]>@
createImageVersion_baseImage :: Lens.Lens' CreateImageVersion Prelude.Text
createImageVersion_baseImage = Lens.lens (\CreateImageVersion' {baseImage} -> baseImage) (\s@CreateImageVersion' {} a -> s {baseImage = a} :: CreateImageVersion)

-- | A unique ID. If not specified, the Amazon Web Services CLI and Amazon
-- Web Services SDKs, such as the SDK for Python (Boto3), add a unique
-- value to the call.
createImageVersion_clientToken :: Lens.Lens' CreateImageVersion Prelude.Text
createImageVersion_clientToken = Lens.lens (\CreateImageVersion' {clientToken} -> clientToken) (\s@CreateImageVersion' {} a -> s {clientToken = a} :: CreateImageVersion)

-- | The @ImageName@ of the @Image@ to create a version of.
createImageVersion_imageName :: Lens.Lens' CreateImageVersion Prelude.Text
createImageVersion_imageName = Lens.lens (\CreateImageVersion' {imageName} -> imageName) (\s@CreateImageVersion' {} a -> s {imageName = a} :: CreateImageVersion)

instance Core.AWSRequest CreateImageVersion where
  type
    AWSResponse CreateImageVersion =
      CreateImageVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateImageVersionResponse'
            Prelude.<$> (x Data..?> "ImageVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateImageVersion where
  hashWithSalt _salt CreateImageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` aliases
      `Prelude.hashWithSalt` horovod
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` mLFramework
      `Prelude.hashWithSalt` processor
      `Prelude.hashWithSalt` programmingLang
      `Prelude.hashWithSalt` releaseNotes
      `Prelude.hashWithSalt` vendorGuidance
      `Prelude.hashWithSalt` baseImage
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` imageName

instance Prelude.NFData CreateImageVersion where
  rnf CreateImageVersion' {..} =
    Prelude.rnf aliases `Prelude.seq`
      Prelude.rnf horovod `Prelude.seq`
        Prelude.rnf jobType `Prelude.seq`
          Prelude.rnf mLFramework `Prelude.seq`
            Prelude.rnf processor `Prelude.seq`
              Prelude.rnf programmingLang `Prelude.seq`
                Prelude.rnf releaseNotes `Prelude.seq`
                  Prelude.rnf vendorGuidance `Prelude.seq`
                    Prelude.rnf baseImage `Prelude.seq`
                      Prelude.rnf clientToken `Prelude.seq`
                        Prelude.rnf imageName

instance Data.ToHeaders CreateImageVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateImageVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateImageVersion where
  toJSON CreateImageVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Aliases" Data..=) Prelude.<$> aliases,
            ("Horovod" Data..=) Prelude.<$> horovod,
            ("JobType" Data..=) Prelude.<$> jobType,
            ("MLFramework" Data..=) Prelude.<$> mLFramework,
            ("Processor" Data..=) Prelude.<$> processor,
            ("ProgrammingLang" Data..=)
              Prelude.<$> programmingLang,
            ("ReleaseNotes" Data..=) Prelude.<$> releaseNotes,
            ("VendorGuidance" Data..=)
              Prelude.<$> vendorGuidance,
            Prelude.Just ("BaseImage" Data..= baseImage),
            Prelude.Just ("ClientToken" Data..= clientToken),
            Prelude.Just ("ImageName" Data..= imageName)
          ]
      )

instance Data.ToPath CreateImageVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateImageVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateImageVersionResponse' smart constructor.
data CreateImageVersionResponse = CreateImageVersionResponse'
  { -- | The ARN of the image version.
    imageVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateImageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageVersionArn', 'createImageVersionResponse_imageVersionArn' - The ARN of the image version.
--
-- 'httpStatus', 'createImageVersionResponse_httpStatus' - The response's http status code.
newCreateImageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateImageVersionResponse
newCreateImageVersionResponse pHttpStatus_ =
  CreateImageVersionResponse'
    { imageVersionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the image version.
createImageVersionResponse_imageVersionArn :: Lens.Lens' CreateImageVersionResponse (Prelude.Maybe Prelude.Text)
createImageVersionResponse_imageVersionArn = Lens.lens (\CreateImageVersionResponse' {imageVersionArn} -> imageVersionArn) (\s@CreateImageVersionResponse' {} a -> s {imageVersionArn = a} :: CreateImageVersionResponse)

-- | The response's http status code.
createImageVersionResponse_httpStatus :: Lens.Lens' CreateImageVersionResponse Prelude.Int
createImageVersionResponse_httpStatus = Lens.lens (\CreateImageVersionResponse' {httpStatus} -> httpStatus) (\s@CreateImageVersionResponse' {} a -> s {httpStatus = a} :: CreateImageVersionResponse)

instance Prelude.NFData CreateImageVersionResponse where
  rnf CreateImageVersionResponse' {..} =
    Prelude.rnf imageVersionArn `Prelude.seq`
      Prelude.rnf httpStatus
