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
-- Module      : Amazonka.SecurityHub.Types.AwsEfsAccessPointRootDirectoryDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEfsAccessPointRootDirectoryDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEfsAccessPointRootDirectoryCreationInfoDetails

-- | Provides information about the directory on the Amazon EFS file system
-- that the access point exposes as the root directory to NFS clients using
-- the access point.
--
-- /See:/ 'newAwsEfsAccessPointRootDirectoryDetails' smart constructor.
data AwsEfsAccessPointRootDirectoryDetails = AwsEfsAccessPointRootDirectoryDetails'
  { -- | Specifies the POSIX IDs and permissions to apply to the access point\'s
    -- root directory.
    creationInfo :: Prelude.Maybe AwsEfsAccessPointRootDirectoryCreationInfoDetails,
    -- | Specifies the path on the Amazon EFS file system to expose as the root
    -- directory to NFS clients using the access point to access the EFS file
    -- system. A path can have up to four subdirectories. If the specified path
    -- does not exist, you are required to provide @CreationInfo@.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEfsAccessPointRootDirectoryDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationInfo', 'awsEfsAccessPointRootDirectoryDetails_creationInfo' - Specifies the POSIX IDs and permissions to apply to the access point\'s
-- root directory.
--
-- 'path', 'awsEfsAccessPointRootDirectoryDetails_path' - Specifies the path on the Amazon EFS file system to expose as the root
-- directory to NFS clients using the access point to access the EFS file
-- system. A path can have up to four subdirectories. If the specified path
-- does not exist, you are required to provide @CreationInfo@.
newAwsEfsAccessPointRootDirectoryDetails ::
  AwsEfsAccessPointRootDirectoryDetails
newAwsEfsAccessPointRootDirectoryDetails =
  AwsEfsAccessPointRootDirectoryDetails'
    { creationInfo =
        Prelude.Nothing,
      path = Prelude.Nothing
    }

-- | Specifies the POSIX IDs and permissions to apply to the access point\'s
-- root directory.
awsEfsAccessPointRootDirectoryDetails_creationInfo :: Lens.Lens' AwsEfsAccessPointRootDirectoryDetails (Prelude.Maybe AwsEfsAccessPointRootDirectoryCreationInfoDetails)
awsEfsAccessPointRootDirectoryDetails_creationInfo = Lens.lens (\AwsEfsAccessPointRootDirectoryDetails' {creationInfo} -> creationInfo) (\s@AwsEfsAccessPointRootDirectoryDetails' {} a -> s {creationInfo = a} :: AwsEfsAccessPointRootDirectoryDetails)

-- | Specifies the path on the Amazon EFS file system to expose as the root
-- directory to NFS clients using the access point to access the EFS file
-- system. A path can have up to four subdirectories. If the specified path
-- does not exist, you are required to provide @CreationInfo@.
awsEfsAccessPointRootDirectoryDetails_path :: Lens.Lens' AwsEfsAccessPointRootDirectoryDetails (Prelude.Maybe Prelude.Text)
awsEfsAccessPointRootDirectoryDetails_path = Lens.lens (\AwsEfsAccessPointRootDirectoryDetails' {path} -> path) (\s@AwsEfsAccessPointRootDirectoryDetails' {} a -> s {path = a} :: AwsEfsAccessPointRootDirectoryDetails)

instance
  Data.FromJSON
    AwsEfsAccessPointRootDirectoryDetails
  where
  parseJSON =
    Data.withObject
      "AwsEfsAccessPointRootDirectoryDetails"
      ( \x ->
          AwsEfsAccessPointRootDirectoryDetails'
            Prelude.<$> (x Data..:? "CreationInfo")
            Prelude.<*> (x Data..:? "Path")
      )

instance
  Prelude.Hashable
    AwsEfsAccessPointRootDirectoryDetails
  where
  hashWithSalt
    _salt
    AwsEfsAccessPointRootDirectoryDetails' {..} =
      _salt `Prelude.hashWithSalt` creationInfo
        `Prelude.hashWithSalt` path

instance
  Prelude.NFData
    AwsEfsAccessPointRootDirectoryDetails
  where
  rnf AwsEfsAccessPointRootDirectoryDetails' {..} =
    Prelude.rnf creationInfo
      `Prelude.seq` Prelude.rnf path

instance
  Data.ToJSON
    AwsEfsAccessPointRootDirectoryDetails
  where
  toJSON AwsEfsAccessPointRootDirectoryDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationInfo" Data..=) Prelude.<$> creationInfo,
            ("Path" Data..=) Prelude.<$> path
          ]
      )
