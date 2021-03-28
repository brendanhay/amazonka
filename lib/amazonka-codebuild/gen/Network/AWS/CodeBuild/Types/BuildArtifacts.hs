{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.BuildArtifacts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.BuildArtifacts
  ( BuildArtifacts (..)
  -- * Smart constructor
  , mkBuildArtifacts
  -- * Lenses
  , baArtifactIdentifier
  , baEncryptionDisabled
  , baLocation
  , baMd5sum
  , baOverrideArtifactName
  , baSha256sum
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about build output artifacts.
--
-- /See:/ 'mkBuildArtifacts' smart constructor.
data BuildArtifacts = BuildArtifacts'
  { artifactIdentifier :: Core.Maybe Core.Text
    -- ^ An identifier for this artifact definition. 
  , encryptionDisabled :: Core.Maybe Core.Bool
    -- ^ Information that tells you if encryption for build artifacts is disabled. 
  , location :: Core.Maybe Core.Text
    -- ^ Information about the location of the build artifacts.
  , md5sum :: Core.Maybe Core.Text
    -- ^ The MD5 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
  , overrideArtifactName :: Core.Maybe Core.Bool
    -- ^ If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique. 
  , sha256sum :: Core.Maybe Core.Text
    -- ^ The SHA-256 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BuildArtifacts' value with any optional fields omitted.
mkBuildArtifacts
    :: BuildArtifacts
mkBuildArtifacts
  = BuildArtifacts'{artifactIdentifier = Core.Nothing,
                    encryptionDisabled = Core.Nothing, location = Core.Nothing,
                    md5sum = Core.Nothing, overrideArtifactName = Core.Nothing,
                    sha256sum = Core.Nothing}

-- | An identifier for this artifact definition. 
--
-- /Note:/ Consider using 'artifactIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baArtifactIdentifier :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Text)
baArtifactIdentifier = Lens.field @"artifactIdentifier"
{-# INLINEABLE baArtifactIdentifier #-}
{-# DEPRECATED artifactIdentifier "Use generic-lens or generic-optics with 'artifactIdentifier' instead"  #-}

-- | Information that tells you if encryption for build artifacts is disabled. 
--
-- /Note:/ Consider using 'encryptionDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baEncryptionDisabled :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Bool)
baEncryptionDisabled = Lens.field @"encryptionDisabled"
{-# INLINEABLE baEncryptionDisabled #-}
{-# DEPRECATED encryptionDisabled "Use generic-lens or generic-optics with 'encryptionDisabled' instead"  #-}

-- | Information about the location of the build artifacts.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baLocation :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Text)
baLocation = Lens.field @"location"
{-# INLINEABLE baLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | The MD5 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
--
-- /Note:/ Consider using 'md5sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baMd5sum :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Text)
baMd5sum = Lens.field @"md5sum"
{-# INLINEABLE baMd5sum #-}
{-# DEPRECATED md5sum "Use generic-lens or generic-optics with 'md5sum' instead"  #-}

-- | If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique. 
--
-- /Note:/ Consider using 'overrideArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baOverrideArtifactName :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Bool)
baOverrideArtifactName = Lens.field @"overrideArtifactName"
{-# INLINEABLE baOverrideArtifactName #-}
{-# DEPRECATED overrideArtifactName "Use generic-lens or generic-optics with 'overrideArtifactName' instead"  #-}

-- | The SHA-256 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
--
-- /Note:/ Consider using 'sha256sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baSha256sum :: Lens.Lens' BuildArtifacts (Core.Maybe Core.Text)
baSha256sum = Lens.field @"sha256sum"
{-# INLINEABLE baSha256sum #-}
{-# DEPRECATED sha256sum "Use generic-lens or generic-optics with 'sha256sum' instead"  #-}

instance Core.FromJSON BuildArtifacts where
        parseJSON
          = Core.withObject "BuildArtifacts" Core.$
              \ x ->
                BuildArtifacts' Core.<$>
                  (x Core..:? "artifactIdentifier") Core.<*>
                    x Core..:? "encryptionDisabled"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "md5sum"
                    Core.<*> x Core..:? "overrideArtifactName"
                    Core.<*> x Core..:? "sha256sum"
