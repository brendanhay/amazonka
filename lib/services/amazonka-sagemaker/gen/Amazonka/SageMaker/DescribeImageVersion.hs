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
-- Module      : Amazonka.SageMaker.DescribeImageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a version of a SageMaker image.
module Amazonka.SageMaker.DescribeImageVersion
  ( -- * Creating a Request
    DescribeImageVersion (..),
    newDescribeImageVersion,

    -- * Request Lenses
    describeImageVersion_alias,
    describeImageVersion_version,
    describeImageVersion_imageName,

    -- * Destructuring the Response
    DescribeImageVersionResponse (..),
    newDescribeImageVersionResponse,

    -- * Response Lenses
    describeImageVersionResponse_baseImage,
    describeImageVersionResponse_containerImage,
    describeImageVersionResponse_creationTime,
    describeImageVersionResponse_failureReason,
    describeImageVersionResponse_horovod,
    describeImageVersionResponse_imageArn,
    describeImageVersionResponse_imageVersionArn,
    describeImageVersionResponse_imageVersionStatus,
    describeImageVersionResponse_jobType,
    describeImageVersionResponse_lastModifiedTime,
    describeImageVersionResponse_mLFramework,
    describeImageVersionResponse_processor,
    describeImageVersionResponse_programmingLang,
    describeImageVersionResponse_releaseNotes,
    describeImageVersionResponse_vendorGuidance,
    describeImageVersionResponse_version,
    describeImageVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeImageVersion' smart constructor.
data DescribeImageVersion = DescribeImageVersion'
  { -- | The alias of the image version.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The version of the image. If not specified, the latest version is
    -- described.
    version :: Prelude.Maybe Prelude.Natural,
    -- | The name of the image.
    imageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'describeImageVersion_alias' - The alias of the image version.
--
-- 'version', 'describeImageVersion_version' - The version of the image. If not specified, the latest version is
-- described.
--
-- 'imageName', 'describeImageVersion_imageName' - The name of the image.
newDescribeImageVersion ::
  -- | 'imageName'
  Prelude.Text ->
  DescribeImageVersion
newDescribeImageVersion pImageName_ =
  DescribeImageVersion'
    { alias = Prelude.Nothing,
      version = Prelude.Nothing,
      imageName = pImageName_
    }

-- | The alias of the image version.
describeImageVersion_alias :: Lens.Lens' DescribeImageVersion (Prelude.Maybe Prelude.Text)
describeImageVersion_alias = Lens.lens (\DescribeImageVersion' {alias} -> alias) (\s@DescribeImageVersion' {} a -> s {alias = a} :: DescribeImageVersion)

-- | The version of the image. If not specified, the latest version is
-- described.
describeImageVersion_version :: Lens.Lens' DescribeImageVersion (Prelude.Maybe Prelude.Natural)
describeImageVersion_version = Lens.lens (\DescribeImageVersion' {version} -> version) (\s@DescribeImageVersion' {} a -> s {version = a} :: DescribeImageVersion)

-- | The name of the image.
describeImageVersion_imageName :: Lens.Lens' DescribeImageVersion Prelude.Text
describeImageVersion_imageName = Lens.lens (\DescribeImageVersion' {imageName} -> imageName) (\s@DescribeImageVersion' {} a -> s {imageName = a} :: DescribeImageVersion)

instance Core.AWSRequest DescribeImageVersion where
  type
    AWSResponse DescribeImageVersion =
      DescribeImageVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeImageVersionResponse'
            Prelude.<$> (x Data..?> "BaseImage")
            Prelude.<*> (x Data..?> "ContainerImage")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "Horovod")
            Prelude.<*> (x Data..?> "ImageArn")
            Prelude.<*> (x Data..?> "ImageVersionArn")
            Prelude.<*> (x Data..?> "ImageVersionStatus")
            Prelude.<*> (x Data..?> "JobType")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "MLFramework")
            Prelude.<*> (x Data..?> "Processor")
            Prelude.<*> (x Data..?> "ProgrammingLang")
            Prelude.<*> (x Data..?> "ReleaseNotes")
            Prelude.<*> (x Data..?> "VendorGuidance")
            Prelude.<*> (x Data..?> "Version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeImageVersion where
  hashWithSalt _salt DescribeImageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` imageName

instance Prelude.NFData DescribeImageVersion where
  rnf DescribeImageVersion' {..} =
    Prelude.rnf alias `Prelude.seq`
      Prelude.rnf version `Prelude.seq`
        Prelude.rnf imageName

instance Data.ToHeaders DescribeImageVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeImageVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeImageVersion where
  toJSON DescribeImageVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Alias" Data..=) Prelude.<$> alias,
            ("Version" Data..=) Prelude.<$> version,
            Prelude.Just ("ImageName" Data..= imageName)
          ]
      )

instance Data.ToPath DescribeImageVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeImageVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeImageVersionResponse' smart constructor.
data DescribeImageVersionResponse = DescribeImageVersionResponse'
  { -- | The registry path of the container image on which this image version is
    -- based.
    baseImage :: Prelude.Maybe Prelude.Text,
    -- | The registry path of the container image that contains this image
    -- version.
    containerImage :: Prelude.Maybe Prelude.Text,
    -- | When the version was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | When a create or delete operation fails, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Indicates Horovod compatibility.
    horovod :: Prelude.Maybe Prelude.Bool,
    -- | The ARN of the image the version is based on.
    imageArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the version.
    imageVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the version.
    imageVersionStatus :: Prelude.Maybe ImageVersionStatus,
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
    -- | When the version was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
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
    -- | The stability of the image version specified by the maintainer.
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
    -- | The version number.
    version :: Prelude.Maybe Prelude.Natural,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeImageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baseImage', 'describeImageVersionResponse_baseImage' - The registry path of the container image on which this image version is
-- based.
--
-- 'containerImage', 'describeImageVersionResponse_containerImage' - The registry path of the container image that contains this image
-- version.
--
-- 'creationTime', 'describeImageVersionResponse_creationTime' - When the version was created.
--
-- 'failureReason', 'describeImageVersionResponse_failureReason' - When a create or delete operation fails, the reason for the failure.
--
-- 'horovod', 'describeImageVersionResponse_horovod' - Indicates Horovod compatibility.
--
-- 'imageArn', 'describeImageVersionResponse_imageArn' - The ARN of the image the version is based on.
--
-- 'imageVersionArn', 'describeImageVersionResponse_imageVersionArn' - The ARN of the version.
--
-- 'imageVersionStatus', 'describeImageVersionResponse_imageVersionStatus' - The status of the version.
--
-- 'jobType', 'describeImageVersionResponse_jobType' - Indicates SageMaker job type compatibility.
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
-- 'lastModifiedTime', 'describeImageVersionResponse_lastModifiedTime' - When the version was last modified.
--
-- 'mLFramework', 'describeImageVersionResponse_mLFramework' - The machine learning framework vended in the image version.
--
-- 'processor', 'describeImageVersionResponse_processor' - Indicates CPU or GPU compatibility.
--
-- -   @CPU@: The image version is compatible with CPU.
--
-- -   @GPU@: The image version is compatible with GPU.
--
-- 'programmingLang', 'describeImageVersionResponse_programmingLang' - The supported programming language and its version.
--
-- 'releaseNotes', 'describeImageVersionResponse_releaseNotes' - The maintainer description of the image version.
--
-- 'vendorGuidance', 'describeImageVersionResponse_vendorGuidance' - The stability of the image version specified by the maintainer.
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
-- 'version', 'describeImageVersionResponse_version' - The version number.
--
-- 'httpStatus', 'describeImageVersionResponse_httpStatus' - The response's http status code.
newDescribeImageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeImageVersionResponse
newDescribeImageVersionResponse pHttpStatus_ =
  DescribeImageVersionResponse'
    { baseImage =
        Prelude.Nothing,
      containerImage = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      horovod = Prelude.Nothing,
      imageArn = Prelude.Nothing,
      imageVersionArn = Prelude.Nothing,
      imageVersionStatus = Prelude.Nothing,
      jobType = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      mLFramework = Prelude.Nothing,
      processor = Prelude.Nothing,
      programmingLang = Prelude.Nothing,
      releaseNotes = Prelude.Nothing,
      vendorGuidance = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry path of the container image on which this image version is
-- based.
describeImageVersionResponse_baseImage :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_baseImage = Lens.lens (\DescribeImageVersionResponse' {baseImage} -> baseImage) (\s@DescribeImageVersionResponse' {} a -> s {baseImage = a} :: DescribeImageVersionResponse)

-- | The registry path of the container image that contains this image
-- version.
describeImageVersionResponse_containerImage :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_containerImage = Lens.lens (\DescribeImageVersionResponse' {containerImage} -> containerImage) (\s@DescribeImageVersionResponse' {} a -> s {containerImage = a} :: DescribeImageVersionResponse)

-- | When the version was created.
describeImageVersionResponse_creationTime :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.UTCTime)
describeImageVersionResponse_creationTime = Lens.lens (\DescribeImageVersionResponse' {creationTime} -> creationTime) (\s@DescribeImageVersionResponse' {} a -> s {creationTime = a} :: DescribeImageVersionResponse) Prelude.. Lens.mapping Data._Time

-- | When a create or delete operation fails, the reason for the failure.
describeImageVersionResponse_failureReason :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_failureReason = Lens.lens (\DescribeImageVersionResponse' {failureReason} -> failureReason) (\s@DescribeImageVersionResponse' {} a -> s {failureReason = a} :: DescribeImageVersionResponse)

-- | Indicates Horovod compatibility.
describeImageVersionResponse_horovod :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Bool)
describeImageVersionResponse_horovod = Lens.lens (\DescribeImageVersionResponse' {horovod} -> horovod) (\s@DescribeImageVersionResponse' {} a -> s {horovod = a} :: DescribeImageVersionResponse)

-- | The ARN of the image the version is based on.
describeImageVersionResponse_imageArn :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_imageArn = Lens.lens (\DescribeImageVersionResponse' {imageArn} -> imageArn) (\s@DescribeImageVersionResponse' {} a -> s {imageArn = a} :: DescribeImageVersionResponse)

-- | The ARN of the version.
describeImageVersionResponse_imageVersionArn :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_imageVersionArn = Lens.lens (\DescribeImageVersionResponse' {imageVersionArn} -> imageVersionArn) (\s@DescribeImageVersionResponse' {} a -> s {imageVersionArn = a} :: DescribeImageVersionResponse)

-- | The status of the version.
describeImageVersionResponse_imageVersionStatus :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe ImageVersionStatus)
describeImageVersionResponse_imageVersionStatus = Lens.lens (\DescribeImageVersionResponse' {imageVersionStatus} -> imageVersionStatus) (\s@DescribeImageVersionResponse' {} a -> s {imageVersionStatus = a} :: DescribeImageVersionResponse)

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
describeImageVersionResponse_jobType :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe JobType)
describeImageVersionResponse_jobType = Lens.lens (\DescribeImageVersionResponse' {jobType} -> jobType) (\s@DescribeImageVersionResponse' {} a -> s {jobType = a} :: DescribeImageVersionResponse)

-- | When the version was last modified.
describeImageVersionResponse_lastModifiedTime :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.UTCTime)
describeImageVersionResponse_lastModifiedTime = Lens.lens (\DescribeImageVersionResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeImageVersionResponse' {} a -> s {lastModifiedTime = a} :: DescribeImageVersionResponse) Prelude.. Lens.mapping Data._Time

-- | The machine learning framework vended in the image version.
describeImageVersionResponse_mLFramework :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_mLFramework = Lens.lens (\DescribeImageVersionResponse' {mLFramework} -> mLFramework) (\s@DescribeImageVersionResponse' {} a -> s {mLFramework = a} :: DescribeImageVersionResponse)

-- | Indicates CPU or GPU compatibility.
--
-- -   @CPU@: The image version is compatible with CPU.
--
-- -   @GPU@: The image version is compatible with GPU.
describeImageVersionResponse_processor :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Processor)
describeImageVersionResponse_processor = Lens.lens (\DescribeImageVersionResponse' {processor} -> processor) (\s@DescribeImageVersionResponse' {} a -> s {processor = a} :: DescribeImageVersionResponse)

-- | The supported programming language and its version.
describeImageVersionResponse_programmingLang :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_programmingLang = Lens.lens (\DescribeImageVersionResponse' {programmingLang} -> programmingLang) (\s@DescribeImageVersionResponse' {} a -> s {programmingLang = a} :: DescribeImageVersionResponse)

-- | The maintainer description of the image version.
describeImageVersionResponse_releaseNotes :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Text)
describeImageVersionResponse_releaseNotes = Lens.lens (\DescribeImageVersionResponse' {releaseNotes} -> releaseNotes) (\s@DescribeImageVersionResponse' {} a -> s {releaseNotes = a} :: DescribeImageVersionResponse)

-- | The stability of the image version specified by the maintainer.
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
describeImageVersionResponse_vendorGuidance :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe VendorGuidance)
describeImageVersionResponse_vendorGuidance = Lens.lens (\DescribeImageVersionResponse' {vendorGuidance} -> vendorGuidance) (\s@DescribeImageVersionResponse' {} a -> s {vendorGuidance = a} :: DescribeImageVersionResponse)

-- | The version number.
describeImageVersionResponse_version :: Lens.Lens' DescribeImageVersionResponse (Prelude.Maybe Prelude.Natural)
describeImageVersionResponse_version = Lens.lens (\DescribeImageVersionResponse' {version} -> version) (\s@DescribeImageVersionResponse' {} a -> s {version = a} :: DescribeImageVersionResponse)

-- | The response's http status code.
describeImageVersionResponse_httpStatus :: Lens.Lens' DescribeImageVersionResponse Prelude.Int
describeImageVersionResponse_httpStatus = Lens.lens (\DescribeImageVersionResponse' {httpStatus} -> httpStatus) (\s@DescribeImageVersionResponse' {} a -> s {httpStatus = a} :: DescribeImageVersionResponse)

instance Prelude.NFData DescribeImageVersionResponse where
  rnf DescribeImageVersionResponse' {..} =
    Prelude.rnf baseImage `Prelude.seq`
      Prelude.rnf containerImage `Prelude.seq`
        Prelude.rnf creationTime `Prelude.seq`
          Prelude.rnf failureReason `Prelude.seq`
            Prelude.rnf horovod `Prelude.seq`
              Prelude.rnf imageArn `Prelude.seq`
                Prelude.rnf imageVersionArn `Prelude.seq`
                  Prelude.rnf imageVersionStatus `Prelude.seq`
                    Prelude.rnf jobType `Prelude.seq`
                      Prelude.rnf lastModifiedTime `Prelude.seq`
                        Prelude.rnf mLFramework `Prelude.seq`
                          Prelude.rnf processor `Prelude.seq`
                            Prelude.rnf programmingLang `Prelude.seq`
                              Prelude.rnf releaseNotes `Prelude.seq`
                                Prelude.rnf vendorGuidance `Prelude.seq`
                                  Prelude.rnf version `Prelude.seq`
                                    Prelude.rnf httpStatus
