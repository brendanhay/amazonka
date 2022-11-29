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
-- Module      : Amazonka.Panorama.Types.PackageVersionInputConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.PackageVersionInputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Panorama.Types.S3Location
import qualified Amazonka.Prelude as Prelude

-- | A package version input configuration.
--
-- /See:/ 'newPackageVersionInputConfig' smart constructor.
data PackageVersionInputConfig = PackageVersionInputConfig'
  { -- | A location in Amazon S3.
    s3Location :: S3Location
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageVersionInputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'packageVersionInputConfig_s3Location' - A location in Amazon S3.
newPackageVersionInputConfig ::
  -- | 's3Location'
  S3Location ->
  PackageVersionInputConfig
newPackageVersionInputConfig pS3Location_ =
  PackageVersionInputConfig'
    { s3Location =
        pS3Location_
    }

-- | A location in Amazon S3.
packageVersionInputConfig_s3Location :: Lens.Lens' PackageVersionInputConfig S3Location
packageVersionInputConfig_s3Location = Lens.lens (\PackageVersionInputConfig' {s3Location} -> s3Location) (\s@PackageVersionInputConfig' {} a -> s {s3Location = a} :: PackageVersionInputConfig)

instance Core.FromJSON PackageVersionInputConfig where
  parseJSON =
    Core.withObject
      "PackageVersionInputConfig"
      ( \x ->
          PackageVersionInputConfig'
            Prelude.<$> (x Core..: "S3Location")
      )

instance Prelude.Hashable PackageVersionInputConfig where
  hashWithSalt _salt PackageVersionInputConfig' {..} =
    _salt `Prelude.hashWithSalt` s3Location

instance Prelude.NFData PackageVersionInputConfig where
  rnf PackageVersionInputConfig' {..} =
    Prelude.rnf s3Location

instance Core.ToJSON PackageVersionInputConfig where
  toJSON PackageVersionInputConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Location" Core..= s3Location)]
      )
