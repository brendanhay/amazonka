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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.JobBookmarksEncryptionMode
import qualified Network.AWS.Lens as Lens

-- | Specifies how job bookmark data should be encrypted.
--
-- /See:/ 'newJobBookmarksEncryption' smart constructor.
data JobBookmarksEncryption = JobBookmarksEncryption'
  { -- | The encryption mode to use for job bookmarks data.
    jobBookmarksEncryptionMode :: Core.Maybe JobBookmarksEncryptionMode,
    -- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
    -- data.
    kmsKeyArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      kmsKeyArn = Core.Nothing
    }

-- | The encryption mode to use for job bookmarks data.
jobBookmarksEncryption_jobBookmarksEncryptionMode :: Lens.Lens' JobBookmarksEncryption (Core.Maybe JobBookmarksEncryptionMode)
jobBookmarksEncryption_jobBookmarksEncryptionMode = Lens.lens (\JobBookmarksEncryption' {jobBookmarksEncryptionMode} -> jobBookmarksEncryptionMode) (\s@JobBookmarksEncryption' {} a -> s {jobBookmarksEncryptionMode = a} :: JobBookmarksEncryption)

-- | The Amazon Resource Name (ARN) of the KMS key to be used to encrypt the
-- data.
jobBookmarksEncryption_kmsKeyArn :: Lens.Lens' JobBookmarksEncryption (Core.Maybe Core.Text)
jobBookmarksEncryption_kmsKeyArn = Lens.lens (\JobBookmarksEncryption' {kmsKeyArn} -> kmsKeyArn) (\s@JobBookmarksEncryption' {} a -> s {kmsKeyArn = a} :: JobBookmarksEncryption)

instance Core.FromJSON JobBookmarksEncryption where
  parseJSON =
    Core.withObject
      "JobBookmarksEncryption"
      ( \x ->
          JobBookmarksEncryption'
            Core.<$> (x Core..:? "JobBookmarksEncryptionMode")
            Core.<*> (x Core..:? "KmsKeyArn")
      )

instance Core.Hashable JobBookmarksEncryption

instance Core.NFData JobBookmarksEncryption

instance Core.ToJSON JobBookmarksEncryption where
  toJSON JobBookmarksEncryption' {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobBookmarksEncryptionMode" Core..=)
              Core.<$> jobBookmarksEncryptionMode,
            ("KmsKeyArn" Core..=) Core.<$> kmsKeyArn
          ]
      )
