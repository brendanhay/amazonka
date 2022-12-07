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
-- Module      : Amazonka.Synthetics.Types.RuntimeVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.RuntimeVersion where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This structure contains information about one canary runtime version.
-- For more information about runtime versions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
--
-- /See:/ 'newRuntimeVersion' smart constructor.
data RuntimeVersion = RuntimeVersion'
  { -- | The date that the runtime version was released.
    releaseDate :: Prelude.Maybe Data.POSIX,
    -- | If this runtime version is deprecated, this value is the date of
    -- deprecation.
    deprecationDate :: Prelude.Maybe Data.POSIX,
    -- | A description of the runtime version, created by Amazon.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the runtime version. For a list of valid runtime versions,
    -- see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
    versionName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RuntimeVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'releaseDate', 'runtimeVersion_releaseDate' - The date that the runtime version was released.
--
-- 'deprecationDate', 'runtimeVersion_deprecationDate' - If this runtime version is deprecated, this value is the date of
-- deprecation.
--
-- 'description', 'runtimeVersion_description' - A description of the runtime version, created by Amazon.
--
-- 'versionName', 'runtimeVersion_versionName' - The name of the runtime version. For a list of valid runtime versions,
-- see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
newRuntimeVersion ::
  RuntimeVersion
newRuntimeVersion =
  RuntimeVersion'
    { releaseDate = Prelude.Nothing,
      deprecationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      versionName = Prelude.Nothing
    }

-- | The date that the runtime version was released.
runtimeVersion_releaseDate :: Lens.Lens' RuntimeVersion (Prelude.Maybe Prelude.UTCTime)
runtimeVersion_releaseDate = Lens.lens (\RuntimeVersion' {releaseDate} -> releaseDate) (\s@RuntimeVersion' {} a -> s {releaseDate = a} :: RuntimeVersion) Prelude.. Lens.mapping Data._Time

-- | If this runtime version is deprecated, this value is the date of
-- deprecation.
runtimeVersion_deprecationDate :: Lens.Lens' RuntimeVersion (Prelude.Maybe Prelude.UTCTime)
runtimeVersion_deprecationDate = Lens.lens (\RuntimeVersion' {deprecationDate} -> deprecationDate) (\s@RuntimeVersion' {} a -> s {deprecationDate = a} :: RuntimeVersion) Prelude.. Lens.mapping Data._Time

-- | A description of the runtime version, created by Amazon.
runtimeVersion_description :: Lens.Lens' RuntimeVersion (Prelude.Maybe Prelude.Text)
runtimeVersion_description = Lens.lens (\RuntimeVersion' {description} -> description) (\s@RuntimeVersion' {} a -> s {description = a} :: RuntimeVersion)

-- | The name of the runtime version. For a list of valid runtime versions,
-- see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
runtimeVersion_versionName :: Lens.Lens' RuntimeVersion (Prelude.Maybe Prelude.Text)
runtimeVersion_versionName = Lens.lens (\RuntimeVersion' {versionName} -> versionName) (\s@RuntimeVersion' {} a -> s {versionName = a} :: RuntimeVersion)

instance Data.FromJSON RuntimeVersion where
  parseJSON =
    Data.withObject
      "RuntimeVersion"
      ( \x ->
          RuntimeVersion'
            Prelude.<$> (x Data..:? "ReleaseDate")
            Prelude.<*> (x Data..:? "DeprecationDate")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "VersionName")
      )

instance Prelude.Hashable RuntimeVersion where
  hashWithSalt _salt RuntimeVersion' {..} =
    _salt `Prelude.hashWithSalt` releaseDate
      `Prelude.hashWithSalt` deprecationDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` versionName

instance Prelude.NFData RuntimeVersion where
  rnf RuntimeVersion' {..} =
    Prelude.rnf releaseDate
      `Prelude.seq` Prelude.rnf deprecationDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf versionName
