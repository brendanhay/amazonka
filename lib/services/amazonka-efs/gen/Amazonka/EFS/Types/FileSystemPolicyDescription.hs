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
-- Module      : Amazonka.EFS.Types.FileSystemPolicyDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.FileSystemPolicyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newFileSystemPolicyDescription' smart constructor.
data FileSystemPolicyDescription = FileSystemPolicyDescription'
  { -- | The JSON formatted @FileSystemPolicy@ for the EFS file system.
    policy :: Prelude.Maybe Prelude.Text,
    -- | Specifies the EFS file system to which the @FileSystemPolicy@ applies.
    fileSystemId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystemPolicyDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'fileSystemPolicyDescription_policy' - The JSON formatted @FileSystemPolicy@ for the EFS file system.
--
-- 'fileSystemId', 'fileSystemPolicyDescription_fileSystemId' - Specifies the EFS file system to which the @FileSystemPolicy@ applies.
newFileSystemPolicyDescription ::
  FileSystemPolicyDescription
newFileSystemPolicyDescription =
  FileSystemPolicyDescription'
    { policy =
        Prelude.Nothing,
      fileSystemId = Prelude.Nothing
    }

-- | The JSON formatted @FileSystemPolicy@ for the EFS file system.
fileSystemPolicyDescription_policy :: Lens.Lens' FileSystemPolicyDescription (Prelude.Maybe Prelude.Text)
fileSystemPolicyDescription_policy = Lens.lens (\FileSystemPolicyDescription' {policy} -> policy) (\s@FileSystemPolicyDescription' {} a -> s {policy = a} :: FileSystemPolicyDescription)

-- | Specifies the EFS file system to which the @FileSystemPolicy@ applies.
fileSystemPolicyDescription_fileSystemId :: Lens.Lens' FileSystemPolicyDescription (Prelude.Maybe Prelude.Text)
fileSystemPolicyDescription_fileSystemId = Lens.lens (\FileSystemPolicyDescription' {fileSystemId} -> fileSystemId) (\s@FileSystemPolicyDescription' {} a -> s {fileSystemId = a} :: FileSystemPolicyDescription)

instance Core.FromJSON FileSystemPolicyDescription where
  parseJSON =
    Core.withObject
      "FileSystemPolicyDescription"
      ( \x ->
          FileSystemPolicyDescription'
            Prelude.<$> (x Core..:? "Policy")
            Prelude.<*> (x Core..:? "FileSystemId")
      )

instance Prelude.Hashable FileSystemPolicyDescription where
  hashWithSalt _salt FileSystemPolicyDescription' {..} =
    _salt `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData FileSystemPolicyDescription where
  rnf FileSystemPolicyDescription' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf fileSystemId
