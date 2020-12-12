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
    baLocation,
    baMd5sum,
    baEncryptionDisabled,
    baOverrideArtifactName,
    baArtifactIdentifier,
    baSha256sum,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about build output artifacts.
--
-- /See:/ 'mkBuildArtifacts' smart constructor.
data BuildArtifacts = BuildArtifacts'
  { location ::
      Lude.Maybe Lude.Text,
    md5sum :: Lude.Maybe Lude.Text,
    encryptionDisabled :: Lude.Maybe Lude.Bool,
    overrideArtifactName :: Lude.Maybe Lude.Bool,
    artifactIdentifier :: Lude.Maybe Lude.Text,
    sha256sum :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BuildArtifacts' with the minimum fields required to make a request.
--
-- * 'artifactIdentifier' - An identifier for this artifact definition.
-- * 'encryptionDisabled' - Information that tells you if encryption for build artifacts is disabled.
-- * 'location' - Information about the location of the build artifacts.
-- * 'md5sum' - The MD5 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
-- * 'overrideArtifactName' - If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
-- * 'sha256sum' - The SHA-256 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
mkBuildArtifacts ::
  BuildArtifacts
mkBuildArtifacts =
  BuildArtifacts'
    { location = Lude.Nothing,
      md5sum = Lude.Nothing,
      encryptionDisabled = Lude.Nothing,
      overrideArtifactName = Lude.Nothing,
      artifactIdentifier = Lude.Nothing,
      sha256sum = Lude.Nothing
    }

-- | Information about the location of the build artifacts.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baLocation :: Lens.Lens' BuildArtifacts (Lude.Maybe Lude.Text)
baLocation = Lens.lens (location :: BuildArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: BuildArtifacts)
{-# DEPRECATED baLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The MD5 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
--
-- /Note:/ Consider using 'md5sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baMd5sum :: Lens.Lens' BuildArtifacts (Lude.Maybe Lude.Text)
baMd5sum = Lens.lens (md5sum :: BuildArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {md5sum = a} :: BuildArtifacts)
{-# DEPRECATED baMd5sum "Use generic-lens or generic-optics with 'md5sum' instead." #-}

-- | Information that tells you if encryption for build artifacts is disabled.
--
-- /Note:/ Consider using 'encryptionDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baEncryptionDisabled :: Lens.Lens' BuildArtifacts (Lude.Maybe Lude.Bool)
baEncryptionDisabled = Lens.lens (encryptionDisabled :: BuildArtifacts -> Lude.Maybe Lude.Bool) (\s a -> s {encryptionDisabled = a} :: BuildArtifacts)
{-# DEPRECATED baEncryptionDisabled "Use generic-lens or generic-optics with 'encryptionDisabled' instead." #-}

-- | If this flag is set, a name specified in the buildspec file overrides the artifact name. The name specified in a buildspec file is calculated at build time and uses the Shell Command Language. For example, you can append a date and time to your artifact name so that it is always unique.
--
-- /Note:/ Consider using 'overrideArtifactName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baOverrideArtifactName :: Lens.Lens' BuildArtifacts (Lude.Maybe Lude.Bool)
baOverrideArtifactName = Lens.lens (overrideArtifactName :: BuildArtifacts -> Lude.Maybe Lude.Bool) (\s a -> s {overrideArtifactName = a} :: BuildArtifacts)
{-# DEPRECATED baOverrideArtifactName "Use generic-lens or generic-optics with 'overrideArtifactName' instead." #-}

-- | An identifier for this artifact definition.
--
-- /Note:/ Consider using 'artifactIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baArtifactIdentifier :: Lens.Lens' BuildArtifacts (Lude.Maybe Lude.Text)
baArtifactIdentifier = Lens.lens (artifactIdentifier :: BuildArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {artifactIdentifier = a} :: BuildArtifacts)
{-# DEPRECATED baArtifactIdentifier "Use generic-lens or generic-optics with 'artifactIdentifier' instead." #-}

-- | The SHA-256 hash of the build artifact.
--
-- You can use this hash along with a checksum tool to confirm file integrity and authenticity.
--
-- /Note:/ Consider using 'sha256sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
baSha256sum :: Lens.Lens' BuildArtifacts (Lude.Maybe Lude.Text)
baSha256sum = Lens.lens (sha256sum :: BuildArtifacts -> Lude.Maybe Lude.Text) (\s a -> s {sha256sum = a} :: BuildArtifacts)
{-# DEPRECATED baSha256sum "Use generic-lens or generic-optics with 'sha256sum' instead." #-}

instance Lude.FromJSON BuildArtifacts where
  parseJSON =
    Lude.withObject
      "BuildArtifacts"
      ( \x ->
          BuildArtifacts'
            Lude.<$> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "md5sum")
            Lude.<*> (x Lude..:? "encryptionDisabled")
            Lude.<*> (x Lude..:? "overrideArtifactName")
            Lude.<*> (x Lude..:? "artifactIdentifier")
            Lude.<*> (x Lude..:? "sha256sum")
      )
