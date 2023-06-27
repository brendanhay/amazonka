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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EFS.Types.FileSystemPolicyDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newFileSystemPolicyDescription' smart constructor.
data FileSystemPolicyDescription = FileSystemPolicyDescription'
  { -- | Specifies the EFS file system to which the @FileSystemPolicy@ applies.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | The JSON formatted @FileSystemPolicy@ for the EFS file system.
    policy :: Prelude.Maybe Prelude.Text
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
-- 'fileSystemId', 'fileSystemPolicyDescription_fileSystemId' - Specifies the EFS file system to which the @FileSystemPolicy@ applies.
--
-- 'policy', 'fileSystemPolicyDescription_policy' - The JSON formatted @FileSystemPolicy@ for the EFS file system.
newFileSystemPolicyDescription ::
  FileSystemPolicyDescription
newFileSystemPolicyDescription =
  FileSystemPolicyDescription'
    { fileSystemId =
        Prelude.Nothing,
      policy = Prelude.Nothing
    }

-- | Specifies the EFS file system to which the @FileSystemPolicy@ applies.
fileSystemPolicyDescription_fileSystemId :: Lens.Lens' FileSystemPolicyDescription (Prelude.Maybe Prelude.Text)
fileSystemPolicyDescription_fileSystemId = Lens.lens (\FileSystemPolicyDescription' {fileSystemId} -> fileSystemId) (\s@FileSystemPolicyDescription' {} a -> s {fileSystemId = a} :: FileSystemPolicyDescription)

-- | The JSON formatted @FileSystemPolicy@ for the EFS file system.
fileSystemPolicyDescription_policy :: Lens.Lens' FileSystemPolicyDescription (Prelude.Maybe Prelude.Text)
fileSystemPolicyDescription_policy = Lens.lens (\FileSystemPolicyDescription' {policy} -> policy) (\s@FileSystemPolicyDescription' {} a -> s {policy = a} :: FileSystemPolicyDescription)

instance Data.FromJSON FileSystemPolicyDescription where
  parseJSON =
    Data.withObject
      "FileSystemPolicyDescription"
      ( \x ->
          FileSystemPolicyDescription'
            Prelude.<$> (x Data..:? "FileSystemId")
            Prelude.<*> (x Data..:? "Policy")
      )

instance Prelude.Hashable FileSystemPolicyDescription where
  hashWithSalt _salt FileSystemPolicyDescription' {..} =
    _salt
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` policy

instance Prelude.NFData FileSystemPolicyDescription where
  rnf FileSystemPolicyDescription' {..} =
    Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf policy
