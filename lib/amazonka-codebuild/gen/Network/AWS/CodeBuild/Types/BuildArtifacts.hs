{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.BuildArtifacts
  ( BuildArtifacts (..),

    -- * Smart constructor
    mkBuildArtifacts,

    -- * Lenses
    baArtifactIdentifier,
    baEncryptionDisabled,
    baLocation,
    baMd5sum,
    baOverrideArtifactName,
    baSha256sum,
  )
where

import qualified Network.AWS.CodeBuild.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about build output artifacts.
--
-- /See:/ 'mkBuildArtifacts' smart constructor.
data BuildArtifacts = BuildArtifacts'
  { -- | An identifier for this artifact definition.
    artifactIdentifier :: Core.Maybe Types.String,
    -- | Information that tells you if encryption for build artifacts is disabled.
    encryptionDisabled :: Core.Maybe Core.Bool,
    -- | Information about the location of the build artifacts.
    location :: Core.Maybe Types.String,
    -- | The MD5 hash of the build artifact.
    --
    -- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
    md5sum :: Core.Maybe Types.String,
    -- | If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
    overrideArtifactName :: Core.Maybe Core.Bool,
    -- | The SHA-256 hash of the build artifact.
    --
    -- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
    sha256sum :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BuildArtifacts' value with any optional fields omitted.
mkBuildArtifacts ::
  BuildArtifacts
mkBuildArtifacts =
  BuildArtifacts'
    { artifactIdentifier = Core.Nothing,
      encryptionDisabled = Core.Nothing,
      location = Core.Nothing,
      md5sum = Core.Nothing,
      overrideArtifactName = Core.Nothing,
      sha256sum = Core.Nothing
    }

-- | An identifier for this artifact definition.
--
-- /Note:/ Consider using 'artifactIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baArtifactIdentifier :: Lens.Lens' BuildArtifacts (Core.Maybe Types.String)
baArtifactIdentifier = Lens.field @"artifactIdentifier"
{-# DEPRECATED baArtifactIdentifier "Use generic-lens or generic-optics with 'artifactIdentifier' instead." #-}

-- | Information that tells you if encryption for build artifacts is disabled.
--
-- /Note:/ Consider using 'encryptionDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baEncryptionDisabled :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Bool)
baEncryptionDisabled = Lens.field @"encryptionDisabled"
{-# DEPRECATED baEncryptionDisabled "Use generic-lens or generic-optics with 'encryptionDisabled' instead." #-}

-- | Information about the location of the build artifacts.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baLocation :: Lens.Lens' BuildArtifacts (Core.Maybe Types.String)
baLocation = Lens.field @"location"
{-# DEPRECATED baLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The MD5 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
--
-- /Note:/ Consider using 'md5sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baMd5sum :: Lens.Lens' BuildArtifacts (Core.Maybe Types.String)
baMd5sum = Lens.field @"md5sum"
{-# DEPRECATED baMd5sum "Use generic-lens or generic-optics with 'md5sum' instead." #-}

-- | If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
--
-- /Note:/ Consider using 'overrideArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baOverrideArtifactName :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Bool)
baOverrideArtifactName = Lens.field @"overrideArtifactName"
{-# DEPRECATED baOverrideArtifactName "Use generic-lens or generic-optics with 'overrideArtifactName' instead." #-}

-- | The SHA-256 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
--
-- /Note:/ Consider using 'sha256sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baSha256sum :: Lens.Lens' BuildArtifacts (Core.Maybe Types.String)
baSha256sum = Lens.field @"sha256sum"
{-# DEPRECATED baSha256sum "Use generic-lens or generic-optics with 'sha256sum' instead." #-}

instance Core.FromJSON BuildArtifacts where
  parseJSON =
    Core.withObject "BuildArtifacts" Core.$
      \x ->
        BuildArtifacts'
          Core.<$> (x Core..:? "artifactIdentifier")
          Core.<*> (x Core..:? "encryptionDisabled")
          Core.<*> (x Core..:? "location")
          Core.<*> (x Core..:? "md5sum")
          Core.<*> (x Core..:? "overrideArtifactName")
          Core.<*> (x Core..:? "sha256sum")
