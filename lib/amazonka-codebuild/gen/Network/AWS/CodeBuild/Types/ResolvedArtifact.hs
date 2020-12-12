{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ResolvedArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ResolvedArtifact
  ( ResolvedArtifact (..),

    -- * Smart constructor
    mkResolvedArtifact,

    -- * Lenses
    raLocation,
    raIdentifier,
    raType,
  )
where

import Network.AWS.CodeBuild.Types.ArtifactsType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a resolved build artifact. A resolve artifact is an artifact that is built and deployed to the destination, such as Amazon Simple Storage Service (Amazon S3).
--
-- /See:/ 'mkResolvedArtifact' smart constructor.
data ResolvedArtifact = ResolvedArtifact'
  { location ::
      Lude.Maybe Lude.Text,
    identifier :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe ArtifactsType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResolvedArtifact' with the minimum fields required to make a request.
--
-- * 'identifier' - The identifier of the artifact.
-- * 'location' - The location of the artifact.
-- * 'type'' - Specifies the type of artifact.
mkResolvedArtifact ::
  ResolvedArtifact
mkResolvedArtifact =
  ResolvedArtifact'
    { location = Lude.Nothing,
      identifier = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The location of the artifact.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raLocation :: Lens.Lens' ResolvedArtifact (Lude.Maybe Lude.Text)
raLocation = Lens.lens (location :: ResolvedArtifact -> Lude.Maybe Lude.Text) (\s a -> s {location = a} :: ResolvedArtifact)
{-# DEPRECATED raLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The identifier of the artifact.
--
-- /Note:/ Consider using 'identifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raIdentifier :: Lens.Lens' ResolvedArtifact (Lude.Maybe Lude.Text)
raIdentifier = Lens.lens (identifier :: ResolvedArtifact -> Lude.Maybe Lude.Text) (\s a -> s {identifier = a} :: ResolvedArtifact)
{-# DEPRECATED raIdentifier "Use generic-lens or generic-optics with 'identifier' instead." #-}

-- | Specifies the type of artifact.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raType :: Lens.Lens' ResolvedArtifact (Lude.Maybe ArtifactsType)
raType = Lens.lens (type' :: ResolvedArtifact -> Lude.Maybe ArtifactsType) (\s a -> s {type' = a} :: ResolvedArtifact)
{-# DEPRECATED raType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON ResolvedArtifact where
  parseJSON =
    Lude.withObject
      "ResolvedArtifact"
      ( \x ->
          ResolvedArtifact'
            Lude.<$> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "identifier")
            Lude.<*> (x Lude..:? "type")
      )
