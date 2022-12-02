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
-- Module      : Amazonka.SageMaker.Types.SharingSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SharingSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.NotebookOutputOption

-- | Specifies options for sharing SageMaker Studio notebooks. These settings
-- are specified as part of @DefaultUserSettings@ when the @CreateDomain@
-- API is called, and as part of @UserSettings@ when the
-- @CreateUserProfile@ API is called. When @SharingSettings@ is not
-- specified, notebook sharing isn\'t allowed.
--
-- /See:/ 'newSharingSettings' smart constructor.
data SharingSettings = SharingSettings'
  { -- | When @NotebookOutputOption@ is @Allowed@, the Amazon S3 bucket used to
    -- store the shared notebook snapshots.
    s3OutputPath :: Prelude.Maybe Prelude.Text,
    -- | Whether to include the notebook cell output when sharing the notebook.
    -- The default is @Disabled@.
    notebookOutputOption :: Prelude.Maybe NotebookOutputOption,
    -- | When @NotebookOutputOption@ is @Allowed@, the Amazon Web Services Key
    -- Management Service (KMS) encryption key ID used to encrypt the notebook
    -- cell output in the Amazon S3 bucket.
    s3KmsKeyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SharingSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3OutputPath', 'sharingSettings_s3OutputPath' - When @NotebookOutputOption@ is @Allowed@, the Amazon S3 bucket used to
-- store the shared notebook snapshots.
--
-- 'notebookOutputOption', 'sharingSettings_notebookOutputOption' - Whether to include the notebook cell output when sharing the notebook.
-- The default is @Disabled@.
--
-- 's3KmsKeyId', 'sharingSettings_s3KmsKeyId' - When @NotebookOutputOption@ is @Allowed@, the Amazon Web Services Key
-- Management Service (KMS) encryption key ID used to encrypt the notebook
-- cell output in the Amazon S3 bucket.
newSharingSettings ::
  SharingSettings
newSharingSettings =
  SharingSettings'
    { s3OutputPath = Prelude.Nothing,
      notebookOutputOption = Prelude.Nothing,
      s3KmsKeyId = Prelude.Nothing
    }

-- | When @NotebookOutputOption@ is @Allowed@, the Amazon S3 bucket used to
-- store the shared notebook snapshots.
sharingSettings_s3OutputPath :: Lens.Lens' SharingSettings (Prelude.Maybe Prelude.Text)
sharingSettings_s3OutputPath = Lens.lens (\SharingSettings' {s3OutputPath} -> s3OutputPath) (\s@SharingSettings' {} a -> s {s3OutputPath = a} :: SharingSettings)

-- | Whether to include the notebook cell output when sharing the notebook.
-- The default is @Disabled@.
sharingSettings_notebookOutputOption :: Lens.Lens' SharingSettings (Prelude.Maybe NotebookOutputOption)
sharingSettings_notebookOutputOption = Lens.lens (\SharingSettings' {notebookOutputOption} -> notebookOutputOption) (\s@SharingSettings' {} a -> s {notebookOutputOption = a} :: SharingSettings)

-- | When @NotebookOutputOption@ is @Allowed@, the Amazon Web Services Key
-- Management Service (KMS) encryption key ID used to encrypt the notebook
-- cell output in the Amazon S3 bucket.
sharingSettings_s3KmsKeyId :: Lens.Lens' SharingSettings (Prelude.Maybe Prelude.Text)
sharingSettings_s3KmsKeyId = Lens.lens (\SharingSettings' {s3KmsKeyId} -> s3KmsKeyId) (\s@SharingSettings' {} a -> s {s3KmsKeyId = a} :: SharingSettings)

instance Data.FromJSON SharingSettings where
  parseJSON =
    Data.withObject
      "SharingSettings"
      ( \x ->
          SharingSettings'
            Prelude.<$> (x Data..:? "S3OutputPath")
            Prelude.<*> (x Data..:? "NotebookOutputOption")
            Prelude.<*> (x Data..:? "S3KmsKeyId")
      )

instance Prelude.Hashable SharingSettings where
  hashWithSalt _salt SharingSettings' {..} =
    _salt `Prelude.hashWithSalt` s3OutputPath
      `Prelude.hashWithSalt` notebookOutputOption
      `Prelude.hashWithSalt` s3KmsKeyId

instance Prelude.NFData SharingSettings where
  rnf SharingSettings' {..} =
    Prelude.rnf s3OutputPath
      `Prelude.seq` Prelude.rnf notebookOutputOption
      `Prelude.seq` Prelude.rnf s3KmsKeyId

instance Data.ToJSON SharingSettings where
  toJSON SharingSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3OutputPath" Data..=) Prelude.<$> s3OutputPath,
            ("NotebookOutputOption" Data..=)
              Prelude.<$> notebookOutputOption,
            ("S3KmsKeyId" Data..=) Prelude.<$> s3KmsKeyId
          ]
      )
