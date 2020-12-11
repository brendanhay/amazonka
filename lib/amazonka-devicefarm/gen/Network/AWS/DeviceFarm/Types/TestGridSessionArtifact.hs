-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionArtifact
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionArtifact
  ( TestGridSessionArtifact (..),

    -- * Smart constructor
    mkTestGridSessionArtifact,

    -- * Lenses
    tgsaUrl,
    tgsaType,
    tgsaFilename,
  )
where

import Network.AWS.DeviceFarm.Types.TestGridSessionArtifactType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Artifacts are video and other files that are produced in the process of running a browser in an automated context.
--
-- /See:/ 'mkTestGridSessionArtifact' smart constructor.
data TestGridSessionArtifact = TestGridSessionArtifact'
  { url ::
      Lude.Maybe Lude.Text,
    type' ::
      Lude.Maybe TestGridSessionArtifactType,
    filename :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestGridSessionArtifact' with the minimum fields required to make a request.
--
-- * 'filename' - The file name of the artifact.
-- * 'type'' - The kind of artifact.
-- * 'url' - A semi-stable URL to the content of the object.
mkTestGridSessionArtifact ::
  TestGridSessionArtifact
mkTestGridSessionArtifact =
  TestGridSessionArtifact'
    { url = Lude.Nothing,
      type' = Lude.Nothing,
      filename = Lude.Nothing
    }

-- | A semi-stable URL to the content of the object.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaUrl :: Lens.Lens' TestGridSessionArtifact (Lude.Maybe Lude.Text)
tgsaUrl = Lens.lens (url :: TestGridSessionArtifact -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: TestGridSessionArtifact)
{-# DEPRECATED tgsaUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The kind of artifact.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaType :: Lens.Lens' TestGridSessionArtifact (Lude.Maybe TestGridSessionArtifactType)
tgsaType = Lens.lens (type' :: TestGridSessionArtifact -> Lude.Maybe TestGridSessionArtifactType) (\s a -> s {type' = a} :: TestGridSessionArtifact)
{-# DEPRECATED tgsaType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The file name of the artifact.
--
-- /Note:/ Consider using 'filename' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaFilename :: Lens.Lens' TestGridSessionArtifact (Lude.Maybe Lude.Text)
tgsaFilename = Lens.lens (filename :: TestGridSessionArtifact -> Lude.Maybe Lude.Text) (\s a -> s {filename = a} :: TestGridSessionArtifact)
{-# DEPRECATED tgsaFilename "Use generic-lens or generic-optics with 'filename' instead." #-}

instance Lude.FromJSON TestGridSessionArtifact where
  parseJSON =
    Lude.withObject
      "TestGridSessionArtifact"
      ( \x ->
          TestGridSessionArtifact'
            Lude.<$> (x Lude..:? "url")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "filename")
      )
