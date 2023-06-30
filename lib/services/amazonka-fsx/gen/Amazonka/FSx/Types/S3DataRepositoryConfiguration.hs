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
-- Module      : Amazonka.FSx.Types.S3DataRepositoryConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.S3DataRepositoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.AutoExportPolicy
import Amazonka.FSx.Types.AutoImportPolicy
import qualified Amazonka.Prelude as Prelude

-- | The configuration for an Amazon S3 data repository linked to an Amazon
-- FSx for Lustre file system with a data repository association. The
-- configuration consists of an @AutoImportPolicy@ that defines which file
-- events on the data repository are automatically imported to the file
-- system and an @AutoExportPolicy@ that defines which file events on the
-- file system are automatically exported to the data repository. File
-- events are when files or directories are added, changed, or deleted on
-- the file system or the data repository.
--
-- Data repository associations on Amazon File Cache don\'t use
-- @S3DataRepositoryConfiguration@ because they don\'t support automatic
-- import or automatic export.
--
-- /See:/ 'newS3DataRepositoryConfiguration' smart constructor.
data S3DataRepositoryConfiguration = S3DataRepositoryConfiguration'
  { -- | Specifies the type of updated objects (new, changed, deleted) that will
    -- be automatically exported from your file system to the linked S3 bucket.
    autoExportPolicy :: Prelude.Maybe AutoExportPolicy,
    -- | Specifies the type of updated objects (new, changed, deleted) that will
    -- be automatically imported from the linked S3 bucket to your file system.
    autoImportPolicy :: Prelude.Maybe AutoImportPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3DataRepositoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoExportPolicy', 's3DataRepositoryConfiguration_autoExportPolicy' - Specifies the type of updated objects (new, changed, deleted) that will
-- be automatically exported from your file system to the linked S3 bucket.
--
-- 'autoImportPolicy', 's3DataRepositoryConfiguration_autoImportPolicy' - Specifies the type of updated objects (new, changed, deleted) that will
-- be automatically imported from the linked S3 bucket to your file system.
newS3DataRepositoryConfiguration ::
  S3DataRepositoryConfiguration
newS3DataRepositoryConfiguration =
  S3DataRepositoryConfiguration'
    { autoExportPolicy =
        Prelude.Nothing,
      autoImportPolicy = Prelude.Nothing
    }

-- | Specifies the type of updated objects (new, changed, deleted) that will
-- be automatically exported from your file system to the linked S3 bucket.
s3DataRepositoryConfiguration_autoExportPolicy :: Lens.Lens' S3DataRepositoryConfiguration (Prelude.Maybe AutoExportPolicy)
s3DataRepositoryConfiguration_autoExportPolicy = Lens.lens (\S3DataRepositoryConfiguration' {autoExportPolicy} -> autoExportPolicy) (\s@S3DataRepositoryConfiguration' {} a -> s {autoExportPolicy = a} :: S3DataRepositoryConfiguration)

-- | Specifies the type of updated objects (new, changed, deleted) that will
-- be automatically imported from the linked S3 bucket to your file system.
s3DataRepositoryConfiguration_autoImportPolicy :: Lens.Lens' S3DataRepositoryConfiguration (Prelude.Maybe AutoImportPolicy)
s3DataRepositoryConfiguration_autoImportPolicy = Lens.lens (\S3DataRepositoryConfiguration' {autoImportPolicy} -> autoImportPolicy) (\s@S3DataRepositoryConfiguration' {} a -> s {autoImportPolicy = a} :: S3DataRepositoryConfiguration)

instance Data.FromJSON S3DataRepositoryConfiguration where
  parseJSON =
    Data.withObject
      "S3DataRepositoryConfiguration"
      ( \x ->
          S3DataRepositoryConfiguration'
            Prelude.<$> (x Data..:? "AutoExportPolicy")
            Prelude.<*> (x Data..:? "AutoImportPolicy")
      )

instance
  Prelude.Hashable
    S3DataRepositoryConfiguration
  where
  hashWithSalt _salt S3DataRepositoryConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` autoExportPolicy
      `Prelude.hashWithSalt` autoImportPolicy

instance Prelude.NFData S3DataRepositoryConfiguration where
  rnf S3DataRepositoryConfiguration' {..} =
    Prelude.rnf autoExportPolicy
      `Prelude.seq` Prelude.rnf autoImportPolicy

instance Data.ToJSON S3DataRepositoryConfiguration where
  toJSON S3DataRepositoryConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutoExportPolicy" Data..=)
              Prelude.<$> autoExportPolicy,
            ("AutoImportPolicy" Data..=)
              Prelude.<$> autoImportPolicy
          ]
      )
