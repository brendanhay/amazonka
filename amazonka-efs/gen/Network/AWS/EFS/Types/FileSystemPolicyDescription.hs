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
-- Module      : Network.AWS.EFS.Types.FileSystemPolicyDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.FileSystemPolicyDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | /See:/ 'newFileSystemPolicyDescription' smart constructor.
data FileSystemPolicyDescription = FileSystemPolicyDescription'
  { -- | Specifies the EFS file system to which the @FileSystemPolicy@ applies.
    fileSystemId :: Prelude.Maybe Prelude.Text,
    -- | The JSON formatted @FileSystemPolicy@ for the EFS file system.
    policy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON FileSystemPolicyDescription where
  parseJSON =
    Prelude.withObject
      "FileSystemPolicyDescription"
      ( \x ->
          FileSystemPolicyDescription'
            Prelude.<$> (x Prelude..:? "FileSystemId")
            Prelude.<*> (x Prelude..:? "Policy")
      )

instance Prelude.Hashable FileSystemPolicyDescription

instance Prelude.NFData FileSystemPolicyDescription
