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
-- Module      : Network.AWS.Glue.Types.JobBookmarksEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobBookmarksEncryption where

import Network.AWS.Glue.Types.JobBookmarksEncryptionMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies how job bookmark data should be encrypted.
--
-- /See:/ 'newJobBookmarksEncryption' smart constructor.
data JobBookmarksEncryption = JobBookmarksEncryption'
  { -- | The encryption mode to use for job bookmarks data.
    jobBookmarksEncryptionMode :: Prelude.Maybe JobBookmarksEncryptionMode,
    -- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
    -- data.
    kmsKeyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JobBookmarksEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobBookmarksEncryptionMode', 'jobBookmarksEncryption_jobBookmarksEncryptionMode' - The encryption mode to use for job bookmarks data.
--
-- 'kmsKeyArn', 'jobBookmarksEncryption_kmsKeyArn' - The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
-- data.
newJobBookmarksEncryption ::
  JobBookmarksEncryption
newJobBookmarksEncryption =
  JobBookmarksEncryption'
    { jobBookmarksEncryptionMode =
        Prelude.Nothing,
      kmsKeyArn = Prelude.Nothing
    }

-- | The encryption mode to use for job bookmarks data.
jobBookmarksEncryption_jobBookmarksEncryptionMode :: Lens.Lens' JobBookmarksEncryption (Prelude.Maybe JobBookmarksEncryptionMode)
jobBookmarksEncryption_jobBookmarksEncryptionMode = Lens.lens (\JobBookmarksEncryption' {jobBookmarksEncryptionMode} -> jobBookmarksEncryptionMode) (\s@JobBookmarksEncryption' {} a -> s {jobBookmarksEncryptionMode = a} :: JobBookmarksEncryption)

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
-- data.
jobBookmarksEncryption_kmsKeyArn :: Lens.Lens' JobBookmarksEncryption (Prelude.Maybe Prelude.Text)
jobBookmarksEncryption_kmsKeyArn = Lens.lens (\JobBookmarksEncryption' {kmsKeyArn} -> kmsKeyArn) (\s@JobBookmarksEncryption' {} a -> s {kmsKeyArn = a} :: JobBookmarksEncryption)

instance Prelude.FromJSON JobBookmarksEncryption where
  parseJSON =
    Prelude.withObject
      "JobBookmarksEncryption"
      ( \x ->
          JobBookmarksEncryption'
            Prelude.<$> (x Prelude..:? "JobBookmarksEncryptionMode")
            Prelude.<*> (x Prelude..:? "KmsKeyArn")
      )

instance Prelude.Hashable JobBookmarksEncryption

instance Prelude.NFData JobBookmarksEncryption

instance Prelude.ToJSON JobBookmarksEncryption where
  toJSON JobBookmarksEncryption' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("JobBookmarksEncryptionMode" Prelude..=)
              Prelude.<$> jobBookmarksEncryptionMode,
            ("KmsKeyArn" Prelude..=) Prelude.<$> kmsKeyArn
          ]
      )
