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
-- Module      : Amazonka.CodeBuild.Types.BuildArtifacts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BuildArtifacts where

import Amazonka.CodeBuild.Types.BucketOwnerAccess
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about build output artifacts.
--
-- /See:/ 'newBuildArtifacts' smart constructor.
data BuildArtifacts = BuildArtifacts'
  { -- | An identifier for this artifact definition.
    artifactIdentifier :: Prelude.Maybe Prelude.Text,
    bucketOwnerAccess :: Prelude.Maybe BucketOwnerAccess,
    -- | Information that tells you if encryption for build artifacts is
    -- disabled.
    encryptionDisabled :: Prelude.Maybe Prelude.Bool,
    -- | Information about the location of the build artifacts.
    location :: Prelude.Maybe Prelude.Text,
    -- | The MD5 hash of the build artifact.
    --
    -- You can use this hash along with a checksum tool to confirm file
    -- integrity and authenticity.
    --
    -- This value is available only if the build project\'s @packaging@ value
    -- is set to @ZIP@.
    md5sum :: Prelude.Maybe Prelude.Text,
    -- | If this flag is set, a name specified in the buildspec file overrides
    -- the artifact name. The name specified in a buildspec file is calculated
    -- at build time and uses the Shell Command Language. For example, you can
    -- append a date and time to your artifact name so that it is always
    -- unique.
    overrideArtifactName :: Prelude.Maybe Prelude.Bool,
    -- | The SHA-256 hash of the build artifact.
    --
    -- You can use this hash along with a checksum tool to confirm file
    -- integrity and authenticity.
    --
    -- This value is available only if the build project\'s @packaging@ value
    -- is set to @ZIP@.
    sha256sum :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuildArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'artifactIdentifier', 'buildArtifacts_artifactIdentifier' - An identifier for this artifact definition.
--
-- 'bucketOwnerAccess', 'buildArtifacts_bucketOwnerAccess' - Undocumented member.
--
-- 'encryptionDisabled', 'buildArtifacts_encryptionDisabled' - Information that tells you if encryption for build artifacts is
-- disabled.
--
-- 'location', 'buildArtifacts_location' - Information about the location of the build artifacts.
--
-- 'md5sum', 'buildArtifacts_md5sum' - The MD5 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file
-- integrity and authenticity.
--
-- This value is available only if the build project\'s @packaging@ value
-- is set to @ZIP@.
--
-- 'overrideArtifactName', 'buildArtifacts_overrideArtifactName' - If this flag is set, a name specified in the buildspec file overrides
-- the artifact name. The name specified in a buildspec file is calculated
-- at build time and uses the Shell Command Language. For example, you can
-- append a date and time to your artifact name so that it is always
-- unique.
--
-- 'sha256sum', 'buildArtifacts_sha256sum' - The SHA-256 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file
-- integrity and authenticity.
--
-- This value is available only if the build project\'s @packaging@ value
-- is set to @ZIP@.
newBuildArtifacts ::
  BuildArtifacts
newBuildArtifacts =
  BuildArtifacts'
    { artifactIdentifier =
        Prelude.Nothing,
      bucketOwnerAccess = Prelude.Nothing,
      encryptionDisabled = Prelude.Nothing,
      location = Prelude.Nothing,
      md5sum = Prelude.Nothing,
      overrideArtifactName = Prelude.Nothing,
      sha256sum = Prelude.Nothing
    }

-- | An identifier for this artifact definition.
buildArtifacts_artifactIdentifier :: Lens.Lens' BuildArtifacts (Prelude.Maybe Prelude.Text)
buildArtifacts_artifactIdentifier = Lens.lens (\BuildArtifacts' {artifactIdentifier} -> artifactIdentifier) (\s@BuildArtifacts' {} a -> s {artifactIdentifier = a} :: BuildArtifacts)

-- | Undocumented member.
buildArtifacts_bucketOwnerAccess :: Lens.Lens' BuildArtifacts (Prelude.Maybe BucketOwnerAccess)
buildArtifacts_bucketOwnerAccess = Lens.lens (\BuildArtifacts' {bucketOwnerAccess} -> bucketOwnerAccess) (\s@BuildArtifacts' {} a -> s {bucketOwnerAccess = a} :: BuildArtifacts)

-- | Information that tells you if encryption for build artifacts is
-- disabled.
buildArtifacts_encryptionDisabled :: Lens.Lens' BuildArtifacts (Prelude.Maybe Prelude.Bool)
buildArtifacts_encryptionDisabled = Lens.lens (\BuildArtifacts' {encryptionDisabled} -> encryptionDisabled) (\s@BuildArtifacts' {} a -> s {encryptionDisabled = a} :: BuildArtifacts)

-- | Information about the location of the build artifacts.
buildArtifacts_location :: Lens.Lens' BuildArtifacts (Prelude.Maybe Prelude.Text)
buildArtifacts_location = Lens.lens (\BuildArtifacts' {location} -> location) (\s@BuildArtifacts' {} a -> s {location = a} :: BuildArtifacts)

-- | The MD5 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file
-- integrity and authenticity.
--
-- This value is available only if the build project\'s @packaging@ value
-- is set to @ZIP@.
buildArtifacts_md5sum :: Lens.Lens' BuildArtifacts (Prelude.Maybe Prelude.Text)
buildArtifacts_md5sum = Lens.lens (\BuildArtifacts' {md5sum} -> md5sum) (\s@BuildArtifacts' {} a -> s {md5sum = a} :: BuildArtifacts)

-- | If this flag is set, a name specified in the buildspec file overrides
-- the artifact name. The name specified in a buildspec file is calculated
-- at build time and uses the Shell Command Language. For example, you can
-- append a date and time to your artifact name so that it is always
-- unique.
buildArtifacts_overrideArtifactName :: Lens.Lens' BuildArtifacts (Prelude.Maybe Prelude.Bool)
buildArtifacts_overrideArtifactName = Lens.lens (\BuildArtifacts' {overrideArtifactName} -> overrideArtifactName) (\s@BuildArtifacts' {} a -> s {overrideArtifactName = a} :: BuildArtifacts)

-- | The SHA-256 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file
-- integrity and authenticity.
--
-- This value is available only if the build project\'s @packaging@ value
-- is set to @ZIP@.
buildArtifacts_sha256sum :: Lens.Lens' BuildArtifacts (Prelude.Maybe Prelude.Text)
buildArtifacts_sha256sum = Lens.lens (\BuildArtifacts' {sha256sum} -> sha256sum) (\s@BuildArtifacts' {} a -> s {sha256sum = a} :: BuildArtifacts)

instance Data.FromJSON BuildArtifacts where
  parseJSON =
    Data.withObject
      "BuildArtifacts"
      ( \x ->
          BuildArtifacts'
            Prelude.<$> (x Data..:? "artifactIdentifier")
            Prelude.<*> (x Data..:? "bucketOwnerAccess")
            Prelude.<*> (x Data..:? "encryptionDisabled")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "md5sum")
            Prelude.<*> (x Data..:? "overrideArtifactName")
            Prelude.<*> (x Data..:? "sha256sum")
      )

instance Prelude.Hashable BuildArtifacts where
  hashWithSalt _salt BuildArtifacts' {..} =
    _salt
      `Prelude.hashWithSalt` artifactIdentifier
      `Prelude.hashWithSalt` bucketOwnerAccess
      `Prelude.hashWithSalt` encryptionDisabled
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` md5sum
      `Prelude.hashWithSalt` overrideArtifactName
      `Prelude.hashWithSalt` sha256sum

instance Prelude.NFData BuildArtifacts where
  rnf BuildArtifacts' {..} =
    Prelude.rnf artifactIdentifier `Prelude.seq`
      Prelude.rnf bucketOwnerAccess `Prelude.seq`
        Prelude.rnf encryptionDisabled `Prelude.seq`
          Prelude.rnf location `Prelude.seq`
            Prelude.rnf md5sum `Prelude.seq`
              Prelude.rnf overrideArtifactName `Prelude.seq`
                Prelude.rnf sha256sum
