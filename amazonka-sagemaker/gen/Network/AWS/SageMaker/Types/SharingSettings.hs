{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    s3KmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | When @NotebookOutputOption@ is @Allowed@, the Amazon S3 bucket used to
    -- store the shared notebook snapshots.
    s3OutputPath :: Prelude.Maybe Prelude.Text,
    -- | Whether to include the notebook cell output when sharing the notebook.
    -- The default is @Disabled@.
    notebookOutputOption :: Prelude.Maybe NotebookOutputOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { s3KmsKeyId = Prelude.Nothing,
      s3OutputPath = Prelude.Nothing,
      notebookOutputOption = Prelude.Nothing
    }

-- | When @NotebookOutputOption@ is @Allowed@, the AWS Key Management Service
-- (KMS) encryption key ID used to encrypt the notebook cell output in the
-- Amazon S3 bucket.
sharingSettings_s3KmsKeyId :: Lens.Lens' SharingSettings (Prelude.Maybe Prelude.Text)
sharingSettings_s3KmsKeyId = Lens.lens (\SharingSettings' {s3KmsKeyId} -> s3KmsKeyId) (\s@SharingSettings' {} a -> s {s3KmsKeyId = a} :: SharingSettings)

-- | When @NotebookOutputOption@ is @Allowed@, the Amazon S3 bucket used to
-- store the shared notebook snapshots.
sharingSettings_s3OutputPath :: Lens.Lens' SharingSettings (Prelude.Maybe Prelude.Text)
sharingSettings_s3OutputPath = Lens.lens (\SharingSettings' {s3OutputPath} -> s3OutputPath) (\s@SharingSettings' {} a -> s {s3OutputPath = a} :: SharingSettings)

-- | Whether to include the notebook cell output when sharing the notebook.
-- The default is @Disabled@.
sharingSettings_notebookOutputOption :: Lens.Lens' SharingSettings (Prelude.Maybe NotebookOutputOption)
sharingSettings_notebookOutputOption = Lens.lens (\SharingSettings' {notebookOutputOption} -> notebookOutputOption) (\s@SharingSettings' {} a -> s {notebookOutputOption = a} :: SharingSettings)

instance Prelude.FromJSON SharingSettings where
  parseJSON =
    Prelude.withObject
      "SharingSettings"
      ( \x ->
          SharingSettings'
            Prelude.<$> (x Prelude..:? "S3KmsKeyId")
            Prelude.<*> (x Prelude..:? "S3OutputPath")
            Prelude.<*> (x Prelude..:? "NotebookOutputOption")
      )

instance Prelude.Hashable SharingSettings

instance Prelude.NFData SharingSettings

instance Prelude.ToJSON SharingSettings where
  toJSON SharingSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("S3KmsKeyId" Prelude..=) Prelude.<$> s3KmsKeyId,
            ("S3OutputPath" Prelude..=) Prelude.<$> s3OutputPath,
            ("NotebookOutputOption" Prelude..=)
              Prelude.<$> notebookOutputOption
          ]
      )
