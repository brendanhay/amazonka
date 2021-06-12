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
-- Module      : Network.AWS.CodeBuild.Types.BuildArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildArtifacts where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about build output artifacts.
--
-- /See:/ 'newBuildArtifacts' smart constructor.
data BuildArtifacts = BuildArtifacts'
  { -- | The SHA-256 hash of the build artifact.
    --
    -- You can use this hash along with a checksum tool to confirm file
    -- integrity and authenticity.
    --
    -- This value is available only if the build project\'s @packaging@ value
    -- is set to @ZIP@.
    sha256sum :: Core.Maybe Core.Text,
    -- | If this flag is set, a name specified in the buildspec file overrides
    -- the artifact name. The name specified in a buildspec file is calculated
    -- at build time and uses the Shell Command Language. For example, you can
    -- append a date and time to your artifact name so that it is always
    -- unique.
    overrideArtifactName :: Core.Maybe Core.Bool,
    -- | An identifier for this artifact definition.
    artifactIdentifier :: Core.Maybe Core.Text,
    -- | The MD5 hash of the build artifact.
    --
    -- You can use this hash along with a checksum tool to confirm file
    -- integrity and authenticity.
    --
    -- This value is available only if the build project\'s @packaging@ value
    -- is set to @ZIP@.
    md5sum :: Core.Maybe Core.Text,
    -- | Information that tells you if encryption for build artifacts is
    -- disabled.
    encryptionDisabled :: Core.Maybe Core.Bool,
    -- | Information about the location of the build artifacts.
    location :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BuildArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sha256sum', 'buildArtifacts_sha256sum' - The SHA-256 hash of the build artifact.
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
-- 'artifactIdentifier', 'buildArtifacts_artifactIdentifier' - An identifier for this artifact definition.
--
-- 'md5sum', 'buildArtifacts_md5sum' - The MD5 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file
-- integrity and authenticity.
--
-- This value is available only if the build project\'s @packaging@ value
-- is set to @ZIP@.
--
-- 'encryptionDisabled', 'buildArtifacts_encryptionDisabled' - Information that tells you if encryption for build artifacts is
-- disabled.
--
-- 'location', 'buildArtifacts_location' - Information about the location of the build artifacts.
newBuildArtifacts ::
  BuildArtifacts
newBuildArtifacts =
  BuildArtifacts'
    { sha256sum = Core.Nothing,
      overrideArtifactName = Core.Nothing,
      artifactIdentifier = Core.Nothing,
      md5sum = Core.Nothing,
      encryptionDisabled = Core.Nothing,
      location = Core.Nothing
    }

-- | The SHA-256 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file
-- integrity and authenticity.
--
-- This value is available only if the build project\'s @packaging@ value
-- is set to @ZIP@.
buildArtifacts_sha256sum :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Text)
buildArtifacts_sha256sum = Lens.lens (\BuildArtifacts' {sha256sum} -> sha256sum) (\s@BuildArtifacts' {} a -> s {sha256sum = a} :: BuildArtifacts)

-- | If this flag is set, a name specified in the buildspec file overrides
-- the artifact name. The name specified in a buildspec file is calculated
-- at build time and uses the Shell Command Language. For example, you can
-- append a date and time to your artifact name so that it is always
-- unique.
buildArtifacts_overrideArtifactName :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Bool)
buildArtifacts_overrideArtifactName = Lens.lens (\BuildArtifacts' {overrideArtifactName} -> overrideArtifactName) (\s@BuildArtifacts' {} a -> s {overrideArtifactName = a} :: BuildArtifacts)

-- | An identifier for this artifact definition.
buildArtifacts_artifactIdentifier :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Text)
buildArtifacts_artifactIdentifier = Lens.lens (\BuildArtifacts' {artifactIdentifier} -> artifactIdentifier) (\s@BuildArtifacts' {} a -> s {artifactIdentifier = a} :: BuildArtifacts)

-- | The MD5 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file
-- integrity and authenticity.
--
-- This value is available only if the build project\'s @packaging@ value
-- is set to @ZIP@.
buildArtifacts_md5sum :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Text)
buildArtifacts_md5sum = Lens.lens (\BuildArtifacts' {md5sum} -> md5sum) (\s@BuildArtifacts' {} a -> s {md5sum = a} :: BuildArtifacts)

-- | Information that tells you if encryption for build artifacts is
-- disabled.
buildArtifacts_encryptionDisabled :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Bool)
buildArtifacts_encryptionDisabled = Lens.lens (\BuildArtifacts' {encryptionDisabled} -> encryptionDisabled) (\s@BuildArtifacts' {} a -> s {encryptionDisabled = a} :: BuildArtifacts)

-- | Information about the location of the build artifacts.
buildArtifacts_location :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Text)
buildArtifacts_location = Lens.lens (\BuildArtifacts' {location} -> location) (\s@BuildArtifacts' {} a -> s {location = a} :: BuildArtifacts)

instance Core.FromJSON BuildArtifacts where
  parseJSON =
    Core.withObject
      "BuildArtifacts"
      ( \x ->
          BuildArtifacts'
            Core.<$> (x Core..:? "sha256sum")
            Core.<*> (x Core..:? "overrideArtifactName")
            Core.<*> (x Core..:? "artifactIdentifier")
            Core.<*> (x Core..:? "md5sum")
            Core.<*> (x Core..:? "encryptionDisabled")
            Core.<*> (x Core..:? "location")
      )

instance Core.Hashable BuildArtifacts

instance Core.NFData BuildArtifacts
