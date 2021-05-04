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
-- Module      : Network.AWS.ElasticSearch.Types.PackageSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.PackageSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The S3 location for importing the package specified as @S3BucketName@
-- and @S3Key@
--
-- /See:/ 'newPackageSource' smart constructor.
data PackageSource = PackageSource'
  { -- | Key (file name) of the package.
    s3Key :: Prelude.Maybe Prelude.Text,
    -- | Name of the bucket containing the package.
    s3BucketName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PackageSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Key', 'packageSource_s3Key' - Key (file name) of the package.
--
-- 's3BucketName', 'packageSource_s3BucketName' - Name of the bucket containing the package.
newPackageSource ::
  PackageSource
newPackageSource =
  PackageSource'
    { s3Key = Prelude.Nothing,
      s3BucketName = Prelude.Nothing
    }

-- | Key (file name) of the package.
packageSource_s3Key :: Lens.Lens' PackageSource (Prelude.Maybe Prelude.Text)
packageSource_s3Key = Lens.lens (\PackageSource' {s3Key} -> s3Key) (\s@PackageSource' {} a -> s {s3Key = a} :: PackageSource)

-- | Name of the bucket containing the package.
packageSource_s3BucketName :: Lens.Lens' PackageSource (Prelude.Maybe Prelude.Text)
packageSource_s3BucketName = Lens.lens (\PackageSource' {s3BucketName} -> s3BucketName) (\s@PackageSource' {} a -> s {s3BucketName = a} :: PackageSource)

instance Prelude.Hashable PackageSource

instance Prelude.NFData PackageSource

instance Prelude.ToJSON PackageSource where
  toJSON PackageSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("S3Key" Prelude..=) Prelude.<$> s3Key,
            ("S3BucketName" Prelude..=)
              Prelude.<$> s3BucketName
          ]
      )
