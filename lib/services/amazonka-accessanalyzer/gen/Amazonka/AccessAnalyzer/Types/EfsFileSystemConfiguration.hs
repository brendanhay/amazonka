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
-- Module      : Amazonka.AccessAnalyzer.Types.EfsFileSystemConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.EfsFileSystemConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The proposed access control configuration for an Amazon EFS file system.
-- You can propose a configuration for a new Amazon EFS file system or an
-- existing Amazon EFS file system that you own by specifying the Amazon
-- EFS policy. For more information, see
-- <https://docs.aws.amazon.com/efs/latest/ug/using-fs.html Using file systems in Amazon EFS>.
--
-- -   If the configuration is for an existing Amazon EFS file system and
--     you do not specify the Amazon EFS policy, then the access preview
--     uses the existing Amazon EFS policy for the file system.
--
-- -   If the access preview is for a new resource and you do not specify
--     the policy, then the access preview assumes an Amazon EFS file
--     system without a policy.
--
-- -   To propose deletion of an existing Amazon EFS file system policy,
--     you can specify an empty string for the Amazon EFS policy.
--
-- /See:/ 'newEfsFileSystemConfiguration' smart constructor.
data EfsFileSystemConfiguration = EfsFileSystemConfiguration'
  { -- | The JSON policy definition to apply to the Amazon EFS file system. For
    -- more information on the elements that make up a file system policy, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies Amazon EFS Resource-based policies>.
    fileSystemPolicy :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EfsFileSystemConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemPolicy', 'efsFileSystemConfiguration_fileSystemPolicy' - The JSON policy definition to apply to the Amazon EFS file system. For
-- more information on the elements that make up a file system policy, see
-- <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies Amazon EFS Resource-based policies>.
newEfsFileSystemConfiguration ::
  EfsFileSystemConfiguration
newEfsFileSystemConfiguration =
  EfsFileSystemConfiguration'
    { fileSystemPolicy =
        Prelude.Nothing
    }

-- | The JSON policy definition to apply to the Amazon EFS file system. For
-- more information on the elements that make up a file system policy, see
-- <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies Amazon EFS Resource-based policies>.
efsFileSystemConfiguration_fileSystemPolicy :: Lens.Lens' EfsFileSystemConfiguration (Prelude.Maybe Prelude.Text)
efsFileSystemConfiguration_fileSystemPolicy = Lens.lens (\EfsFileSystemConfiguration' {fileSystemPolicy} -> fileSystemPolicy) (\s@EfsFileSystemConfiguration' {} a -> s {fileSystemPolicy = a} :: EfsFileSystemConfiguration)

instance Data.FromJSON EfsFileSystemConfiguration where
  parseJSON =
    Data.withObject
      "EfsFileSystemConfiguration"
      ( \x ->
          EfsFileSystemConfiguration'
            Prelude.<$> (x Data..:? "fileSystemPolicy")
      )

instance Prelude.Hashable EfsFileSystemConfiguration where
  hashWithSalt _salt EfsFileSystemConfiguration' {..} =
    _salt `Prelude.hashWithSalt` fileSystemPolicy

instance Prelude.NFData EfsFileSystemConfiguration where
  rnf EfsFileSystemConfiguration' {..} =
    Prelude.rnf fileSystemPolicy

instance Data.ToJSON EfsFileSystemConfiguration where
  toJSON EfsFileSystemConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("fileSystemPolicy" Data..=)
              Prelude.<$> fileSystemPolicy
          ]
      )
