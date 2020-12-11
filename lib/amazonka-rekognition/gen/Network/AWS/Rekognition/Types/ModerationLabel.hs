-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.ModerationLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.ModerationLabel
  ( ModerationLabel (..),

    -- * Smart constructor
    mkModerationLabel,

    -- * Lenses
    mlConfidence,
    mlName,
    mlParentName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a single type of unsafe content found in an image or video. Each type of moderated content has a label within a hierarchical taxonomy. For more information, see Detecting Unsafe Content in the Amazon Rekognition Developer Guide.
--
-- /See:/ 'mkModerationLabel' smart constructor.
data ModerationLabel = ModerationLabel'
  { confidence ::
      Lude.Maybe Lude.Double,
    name :: Lude.Maybe Lude.Text,
    parentName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModerationLabel' with the minimum fields required to make a request.
--
-- * 'confidence' - Specifies the confidence that Amazon Rekognition has that the label has been correctly identified.
--
-- If you don't specify the @MinConfidence@ parameter in the call to @DetectModerationLabels@ , the operation returns labels with a confidence value greater than or equal to 50 percent.
-- * 'name' - The label name for the type of unsafe content detected in the image.
-- * 'parentName' - The name for the parent label. Labels at the top level of the hierarchy have the parent label @""@ .
mkModerationLabel ::
  ModerationLabel
mkModerationLabel =
  ModerationLabel'
    { confidence = Lude.Nothing,
      name = Lude.Nothing,
      parentName = Lude.Nothing
    }

-- | Specifies the confidence that Amazon Rekognition has that the label has been correctly identified.
--
-- If you don't specify the @MinConfidence@ parameter in the call to @DetectModerationLabels@ , the operation returns labels with a confidence value greater than or equal to 50 percent.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlConfidence :: Lens.Lens' ModerationLabel (Lude.Maybe Lude.Double)
mlConfidence = Lens.lens (confidence :: ModerationLabel -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: ModerationLabel)
{-# DEPRECATED mlConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The label name for the type of unsafe content detected in the image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlName :: Lens.Lens' ModerationLabel (Lude.Maybe Lude.Text)
mlName = Lens.lens (name :: ModerationLabel -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: ModerationLabel)
{-# DEPRECATED mlName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name for the parent label. Labels at the top level of the hierarchy have the parent label @""@ .
--
-- /Note:/ Consider using 'parentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mlParentName :: Lens.Lens' ModerationLabel (Lude.Maybe Lude.Text)
mlParentName = Lens.lens (parentName :: ModerationLabel -> Lude.Maybe Lude.Text) (\s a -> s {parentName = a} :: ModerationLabel)
{-# DEPRECATED mlParentName "Use generic-lens or generic-optics with 'parentName' instead." #-}

instance Lude.FromJSON ModerationLabel where
  parseJSON =
    Lude.withObject
      "ModerationLabel"
      ( \x ->
          ModerationLabel'
            Lude.<$> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ParentName")
      )
