{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.SharingSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SharingSettings where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.NotebookOutputOption

-- | Specifies options when sharing an Amazon SageMaker Studio notebook.
-- These settings are specified as part of @DefaultUserSettings@ when the
-- CreateDomain API is called, and as part of @UserSettings@ when the
-- CreateUserProfile API is called.
--
-- /See:/ 'newSharingSettings' smart constructor.
data SharingSettings = SharingSettings'
  { -- | When @NotebookOutputOption@ is @Allowed@, the AWS Key Management Service
    -- (KMS) encryption key ID used to encrypt the notebook cell output in the
    -- Amazon S3 bucket.
    s3KmsKeyId :: Core.Maybe Core.Text,
    -- | When @NotebookOutputOption@ is @Allowed@, the Amazon S3 bucket used to
    -- store the shared notebook snapshots.
    s3OutputPath :: Core.Maybe Core.Text,
    -- | Whether to include the notebook cell output when sharing the notebook.
    -- The default is @Disabled@.
    notebookOutputOption :: Core.Maybe NotebookOutputOption
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SharingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3KmsKeyId', 'sharingSettings_s3KmsKeyId' - When @NotebookOutputOption@ is @Allowed@, the AWS Key Management Service
-- (KMS) encryption key ID used to encrypt the notebook cell output in the
-- Amazon S3 bucket.
--
-- 's3OutputPath', 'sharingSettings_s3OutputPath' - When @NotebookOutputOption@ is @Allowed@, the Amazon S3 bucket used to
-- store the shared notebook snapshots.
--
-- 'notebookOutputOption', 'sharingSettings_notebookOutputOption' - Whether to include the notebook cell output when sharing the notebook.
-- The default is @Disabled@.
newSharingSettings ::
  SharingSettings
newSharingSettings =
  SharingSettings'
    { s3KmsKeyId = Core.Nothing,
      s3OutputPath = Core.Nothing,
      notebookOutputOption = Core.Nothing
    }

-- | When @NotebookOutputOption@ is @Allowed@, the AWS Key Management Service
-- (KMS) encryption key ID used to encrypt the notebook cell output in the
-- Amazon S3 bucket.
sharingSettings_s3KmsKeyId :: Lens.Lens' SharingSettings (Core.Maybe Core.Text)
sharingSettings_s3KmsKeyId = Lens.lens (\SharingSettings' {s3KmsKeyId} -> s3KmsKeyId) (\s@SharingSettings' {} a -> s {s3KmsKeyId = a} :: SharingSettings)

-- | When @NotebookOutputOption@ is @Allowed@, the Amazon S3 bucket used to
-- store the shared notebook snapshots.
sharingSettings_s3OutputPath :: Lens.Lens' SharingSettings (Core.Maybe Core.Text)
sharingSettings_s3OutputPath = Lens.lens (\SharingSettings' {s3OutputPath} -> s3OutputPath) (\s@SharingSettings' {} a -> s {s3OutputPath = a} :: SharingSettings)

-- | Whether to include the notebook cell output when sharing the notebook.
-- The default is @Disabled@.
sharingSettings_notebookOutputOption :: Lens.Lens' SharingSettings (Core.Maybe NotebookOutputOption)
sharingSettings_notebookOutputOption = Lens.lens (\SharingSettings' {notebookOutputOption} -> notebookOutputOption) (\s@SharingSettings' {} a -> s {notebookOutputOption = a} :: SharingSettings)

instance Core.FromJSON SharingSettings where
  parseJSON =
    Core.withObject
      "SharingSettings"
      ( \x ->
          SharingSettings'
            Core.<$> (x Core..:? "S3KmsKeyId")
            Core.<*> (x Core..:? "S3OutputPath")
            Core.<*> (x Core..:? "NotebookOutputOption")
      )

instance Core.Hashable SharingSettings

instance Core.NFData SharingSettings

instance Core.ToJSON SharingSettings where
  toJSON SharingSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("S3KmsKeyId" Core..=) Core.<$> s3KmsKeyId,
            ("S3OutputPath" Core..=) Core.<$> s3OutputPath,
            ("NotebookOutputOption" Core..=)
              Core.<$> notebookOutputOption
          ]
      )
