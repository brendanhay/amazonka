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
-- Module      : Amazonka.SageMaker.UpdateImageVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the properties of a SageMaker image version.
module Amazonka.SageMaker.UpdateImageVersion
  ( -- * Creating a Request
    UpdateImageVersion (..),
    newUpdateImageVersion,

    -- * Request Lenses
    updateImageVersion_alias,
    updateImageVersion_aliasesToAdd,
    updateImageVersion_aliasesToDelete,
    updateImageVersion_horovod,
    updateImageVersion_jobType,
    updateImageVersion_mLFramework,
    updateImageVersion_processor,
    updateImageVersion_programmingLang,
    updateImageVersion_releaseNotes,
    updateImageVersion_vendorGuidance,
    updateImageVersion_version,
    updateImageVersion_imageName,

    -- * Destructuring the Response
    UpdateImageVersionResponse (..),
    newUpdateImageVersionResponse,

    -- * Response Lenses
    updateImageVersionResponse_imageVersionArn,
    updateImageVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateImageVersion' smart constructor.
data UpdateImageVersion = UpdateImageVersion'
  { -- | The alias of the image version.
    alias :: Prelude.Maybe Prelude.Text,
    -- | A list of aliases to add.
    aliasesToAdd :: Prelude.Maybe [Prelude.Text],
    -- | A list of aliases to delete.
    aliasesToDelete :: Prelude.Maybe [Prelude.Text],
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
    -- | The availability of the image version specified by the maintainer.
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
    -- | The version of the image.
    version :: Prelude.Maybe Prelude.Natural,
    -- | The name of the image.
    imageName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateImageVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'updateImageVersion_alias' - The alias of the image version.
--
-- 'aliasesToAdd', 'updateImageVersion_aliasesToAdd' - A list of aliases to add.
--
-- 'aliasesToDelete', 'updateImageVersion_aliasesToDelete' - A list of aliases to delete.
--
-- 'horovod', 'updateImageVersion_horovod' - Indicates Horovod compatibility.
--
-- 'jobType', 'updateImageVersion_jobType' - Indicates SageMaker job type compatibility.
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
-- 'mLFramework', 'updateImageVersion_mLFramework' - The machine learning framework vended in the image version.
--
-- 'processor', 'updateImageVersion_processor' - Indicates CPU or GPU compatibility.
--
-- -   @CPU@: The image version is compatible with CPU.
--
-- -   @GPU@: The image version is compatible with GPU.
--
-- 'programmingLang', 'updateImageVersion_programmingLang' - The supported programming language and its version.
--
-- 'releaseNotes', 'updateImageVersion_releaseNotes' - The maintainer description of the image version.
--
-- 'vendorGuidance', 'updateImageVersion_vendorGuidance' - The availability of the image version specified by the maintainer.
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
-- 'version', 'updateImageVersion_version' - The version of the image.
--
-- 'imageName', 'updateImageVersion_imageName' - The name of the image.
newUpdateImageVersion ::
  -- | 'imageName'
  Prelude.Text ->
  UpdateImageVersion
newUpdateImageVersion pImageName_ =
  UpdateImageVersion'
    { alias = Prelude.Nothing,
      aliasesToAdd = Prelude.Nothing,
      aliasesToDelete = Prelude.Nothing,
      horovod = Prelude.Nothing,
      jobType = Prelude.Nothing,
      mLFramework = Prelude.Nothing,
      processor = Prelude.Nothing,
      programmingLang = Prelude.Nothing,
      releaseNotes = Prelude.Nothing,
      vendorGuidance = Prelude.Nothing,
      version = Prelude.Nothing,
      imageName = pImageName_
    }

-- | The alias of the image version.
updateImageVersion_alias :: Lens.Lens' UpdateImageVersion (Prelude.Maybe Prelude.Text)
updateImageVersion_alias = Lens.lens (\UpdateImageVersion' {alias} -> alias) (\s@UpdateImageVersion' {} a -> s {alias = a} :: UpdateImageVersion)

-- | A list of aliases to add.
updateImageVersion_aliasesToAdd :: Lens.Lens' UpdateImageVersion (Prelude.Maybe [Prelude.Text])
updateImageVersion_aliasesToAdd = Lens.lens (\UpdateImageVersion' {aliasesToAdd} -> aliasesToAdd) (\s@UpdateImageVersion' {} a -> s {aliasesToAdd = a} :: UpdateImageVersion) Prelude.. Lens.mapping Lens.coerced

-- | A list of aliases to delete.
updateImageVersion_aliasesToDelete :: Lens.Lens' UpdateImageVersion (Prelude.Maybe [Prelude.Text])
updateImageVersion_aliasesToDelete = Lens.lens (\UpdateImageVersion' {aliasesToDelete} -> aliasesToDelete) (\s@UpdateImageVersion' {} a -> s {aliasesToDelete = a} :: UpdateImageVersion) Prelude.. Lens.mapping Lens.coerced

-- | Indicates Horovod compatibility.
updateImageVersion_horovod :: Lens.Lens' UpdateImageVersion (Prelude.Maybe Prelude.Bool)
updateImageVersion_horovod = Lens.lens (\UpdateImageVersion' {horovod} -> horovod) (\s@UpdateImageVersion' {} a -> s {horovod = a} :: UpdateImageVersion)

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
updateImageVersion_jobType :: Lens.Lens' UpdateImageVersion (Prelude.Maybe JobType)
updateImageVersion_jobType = Lens.lens (\UpdateImageVersion' {jobType} -> jobType) (\s@UpdateImageVersion' {} a -> s {jobType = a} :: UpdateImageVersion)

-- | The machine learning framework vended in the image version.
updateImageVersion_mLFramework :: Lens.Lens' UpdateImageVersion (Prelude.Maybe Prelude.Text)
updateImageVersion_mLFramework = Lens.lens (\UpdateImageVersion' {mLFramework} -> mLFramework) (\s@UpdateImageVersion' {} a -> s {mLFramework = a} :: UpdateImageVersion)

-- | Indicates CPU or GPU compatibility.
--
-- -   @CPU@: The image version is compatible with CPU.
--
-- -   @GPU@: The image version is compatible with GPU.
updateImageVersion_processor :: Lens.Lens' UpdateImageVersion (Prelude.Maybe Processor)
updateImageVersion_processor = Lens.lens (\UpdateImageVersion' {processor} -> processor) (\s@UpdateImageVersion' {} a -> s {processor = a} :: UpdateImageVersion)

-- | The supported programming language and its version.
updateImageVersion_programmingLang :: Lens.Lens' UpdateImageVersion (Prelude.Maybe Prelude.Text)
updateImageVersion_programmingLang = Lens.lens (\UpdateImageVersion' {programmingLang} -> programmingLang) (\s@UpdateImageVersion' {} a -> s {programmingLang = a} :: UpdateImageVersion)

-- | The maintainer description of the image version.
updateImageVersion_releaseNotes :: Lens.Lens' UpdateImageVersion (Prelude.Maybe Prelude.Text)
updateImageVersion_releaseNotes = Lens.lens (\UpdateImageVersion' {releaseNotes} -> releaseNotes) (\s@UpdateImageVersion' {} a -> s {releaseNotes = a} :: UpdateImageVersion)

-- | The availability of the image version specified by the maintainer.
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
updateImageVersion_vendorGuidance :: Lens.Lens' UpdateImageVersion (Prelude.Maybe VendorGuidance)
updateImageVersion_vendorGuidance = Lens.lens (\UpdateImageVersion' {vendorGuidance} -> vendorGuidance) (\s@UpdateImageVersion' {} a -> s {vendorGuidance = a} :: UpdateImageVersion)

-- | The version of the image.
updateImageVersion_version :: Lens.Lens' UpdateImageVersion (Prelude.Maybe Prelude.Natural)
updateImageVersion_version = Lens.lens (\UpdateImageVersion' {version} -> version) (\s@UpdateImageVersion' {} a -> s {version = a} :: UpdateImageVersion)

-- | The name of the image.
updateImageVersion_imageName :: Lens.Lens' UpdateImageVersion Prelude.Text
updateImageVersion_imageName = Lens.lens (\UpdateImageVersion' {imageName} -> imageName) (\s@UpdateImageVersion' {} a -> s {imageName = a} :: UpdateImageVersion)

instance Core.AWSRequest UpdateImageVersion where
  type
    AWSResponse UpdateImageVersion =
      UpdateImageVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateImageVersionResponse'
            Prelude.<$> (x Data..?> "ImageVersionArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateImageVersion where
  hashWithSalt _salt UpdateImageVersion' {..} =
    _salt
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` aliasesToAdd
      `Prelude.hashWithSalt` aliasesToDelete
      `Prelude.hashWithSalt` horovod
      `Prelude.hashWithSalt` jobType
      `Prelude.hashWithSalt` mLFramework
      `Prelude.hashWithSalt` processor
      `Prelude.hashWithSalt` programmingLang
      `Prelude.hashWithSalt` releaseNotes
      `Prelude.hashWithSalt` vendorGuidance
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` imageName

instance Prelude.NFData UpdateImageVersion where
  rnf UpdateImageVersion' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf aliasesToAdd
      `Prelude.seq` Prelude.rnf aliasesToDelete
      `Prelude.seq` Prelude.rnf horovod
      `Prelude.seq` Prelude.rnf jobType
      `Prelude.seq` Prelude.rnf mLFramework
      `Prelude.seq` Prelude.rnf processor
      `Prelude.seq` Prelude.rnf programmingLang
      `Prelude.seq` Prelude.rnf releaseNotes
      `Prelude.seq` Prelude.rnf vendorGuidance
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf imageName

instance Data.ToHeaders UpdateImageVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateImageVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateImageVersion where
  toJSON UpdateImageVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Alias" Data..=) Prelude.<$> alias,
            ("AliasesToAdd" Data..=) Prelude.<$> aliasesToAdd,
            ("AliasesToDelete" Data..=)
              Prelude.<$> aliasesToDelete,
            ("Horovod" Data..=) Prelude.<$> horovod,
            ("JobType" Data..=) Prelude.<$> jobType,
            ("MLFramework" Data..=) Prelude.<$> mLFramework,
            ("Processor" Data..=) Prelude.<$> processor,
            ("ProgrammingLang" Data..=)
              Prelude.<$> programmingLang,
            ("ReleaseNotes" Data..=) Prelude.<$> releaseNotes,
            ("VendorGuidance" Data..=)
              Prelude.<$> vendorGuidance,
            ("Version" Data..=) Prelude.<$> version,
            Prelude.Just ("ImageName" Data..= imageName)
          ]
      )

instance Data.ToPath UpdateImageVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateImageVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateImageVersionResponse' smart constructor.
data UpdateImageVersionResponse = UpdateImageVersionResponse'
  { -- | The ARN of the image version.
    imageVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateImageVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageVersionArn', 'updateImageVersionResponse_imageVersionArn' - The ARN of the image version.
--
-- 'httpStatus', 'updateImageVersionResponse_httpStatus' - The response's http status code.
newUpdateImageVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateImageVersionResponse
newUpdateImageVersionResponse pHttpStatus_ =
  UpdateImageVersionResponse'
    { imageVersionArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the image version.
updateImageVersionResponse_imageVersionArn :: Lens.Lens' UpdateImageVersionResponse (Prelude.Maybe Prelude.Text)
updateImageVersionResponse_imageVersionArn = Lens.lens (\UpdateImageVersionResponse' {imageVersionArn} -> imageVersionArn) (\s@UpdateImageVersionResponse' {} a -> s {imageVersionArn = a} :: UpdateImageVersionResponse)

-- | The response's http status code.
updateImageVersionResponse_httpStatus :: Lens.Lens' UpdateImageVersionResponse Prelude.Int
updateImageVersionResponse_httpStatus = Lens.lens (\UpdateImageVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateImageVersionResponse' {} a -> s {httpStatus = a} :: UpdateImageVersionResponse)

instance Prelude.NFData UpdateImageVersionResponse where
  rnf UpdateImageVersionResponse' {..} =
    Prelude.rnf imageVersionArn
      `Prelude.seq` Prelude.rnf httpStatus
