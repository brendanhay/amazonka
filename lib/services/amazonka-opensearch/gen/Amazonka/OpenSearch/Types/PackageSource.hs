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
-- Module      : Amazonka.OpenSearch.Types.PackageSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.PackageSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 location to import the package from.
--
-- /See:/ 'newPackageSource' smart constructor.
data PackageSource = PackageSource'
  { -- | The name of the Amazon S3 bucket containing the package.
    s3BucketName :: Prelude.Maybe Prelude.Text,
    -- | Key (file name) of the package.
    s3Key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3BucketName', 'packageSource_s3BucketName' - The name of the Amazon S3 bucket containing the package.
--
-- 's3Key', 'packageSource_s3Key' - Key (file name) of the package.
newPackageSource ::
  PackageSource
newPackageSource =
  PackageSource'
    { s3BucketName = Prelude.Nothing,
      s3Key = Prelude.Nothing
    }

-- | The name of the Amazon S3 bucket containing the package.
packageSource_s3BucketName :: Lens.Lens' PackageSource (Prelude.Maybe Prelude.Text)
packageSource_s3BucketName = Lens.lens (\PackageSource' {s3BucketName} -> s3BucketName) (\s@PackageSource' {} a -> s {s3BucketName = a} :: PackageSource)

-- | Key (file name) of the package.
packageSource_s3Key :: Lens.Lens' PackageSource (Prelude.Maybe Prelude.Text)
packageSource_s3Key = Lens.lens (\PackageSource' {s3Key} -> s3Key) (\s@PackageSource' {} a -> s {s3Key = a} :: PackageSource)

instance Prelude.Hashable PackageSource where
  hashWithSalt _salt PackageSource' {..} =
    _salt
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` s3Key

instance Prelude.NFData PackageSource where
  rnf PackageSource' {..} =
    Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf s3Key

instance Data.ToJSON PackageSource where
  toJSON PackageSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("S3BucketName" Data..=) Prelude.<$> s3BucketName,
            ("S3Key" Data..=) Prelude.<$> s3Key
          ]
      )
