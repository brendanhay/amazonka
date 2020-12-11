-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.Types.Artifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ImportExport.Types.Artifact
  ( Artifact (..),

    -- * Smart constructor
    mkArtifact,

    -- * Lenses
    aURL,
    aDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A discrete item that contains the description and URL of an artifact (such as a PDF).
--
-- /See:/ 'mkArtifact' smart constructor.
data Artifact = Artifact'
  { url :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Artifact' with the minimum fields required to make a request.
--
-- * 'description' - Undocumented field.
-- * 'url' - Undocumented field.
mkArtifact ::
  Artifact
mkArtifact =
  Artifact' {url = Lude.Nothing, description = Lude.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aURL :: Lens.Lens' Artifact (Lude.Maybe Lude.Text)
aURL = Lens.lens (url :: Artifact -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: Artifact)
{-# DEPRECATED aURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDescription :: Lens.Lens' Artifact (Lude.Maybe Lude.Text)
aDescription = Lens.lens (description :: Artifact -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Artifact)
{-# DEPRECATED aDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromXML Artifact where
  parseXML x =
    Artifact'
      Lude.<$> (x Lude..@? "URL") Lude.<*> (x Lude..@? "Description")
