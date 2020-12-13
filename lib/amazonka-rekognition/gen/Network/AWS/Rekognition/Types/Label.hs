{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Label
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Label
  ( Label (..),

    -- * Smart constructor
    mkLabel,

    -- * Lenses
    lConfidence,
    lParents,
    lName,
    lInstances,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.Instance
import Network.AWS.Rekognition.Types.Parent

-- | Structure containing details about the detected label, including the name, detected instances, parent labels, and level of confidence.
--
--
--
-- /See:/ 'mkLabel' smart constructor.
data Label = Label'
  { -- | Level of confidence.
    confidence :: Lude.Maybe Lude.Double,
    -- | The parent labels for a label. The response includes all ancestor labels.
    parents :: Lude.Maybe [Parent],
    -- | The name (label) of the object or scene.
    name :: Lude.Maybe Lude.Text,
    -- | If @Label@ represents an object, @Instances@ contains the bounding boxes for each instance of the detected object. Bounding boxes are returned for common object labels such as people, cars, furniture, apparel or pets.
    instances :: Lude.Maybe [Instance]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Label' with the minimum fields required to make a request.
--
-- * 'confidence' - Level of confidence.
-- * 'parents' - The parent labels for a label. The response includes all ancestor labels.
-- * 'name' - The name (label) of the object or scene.
-- * 'instances' - If @Label@ represents an object, @Instances@ contains the bounding boxes for each instance of the detected object. Bounding boxes are returned for common object labels such as people, cars, furniture, apparel or pets.
mkLabel ::
  Label
mkLabel =
  Label'
    { confidence = Lude.Nothing,
      parents = Lude.Nothing,
      name = Lude.Nothing,
      instances = Lude.Nothing
    }

-- | Level of confidence.
--
-- /Note:/ Consider using 'confidence' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lConfidence :: Lens.Lens' Label (Lude.Maybe Lude.Double)
lConfidence = Lens.lens (confidence :: Label -> Lude.Maybe Lude.Double) (\s a -> s {confidence = a} :: Label)
{-# DEPRECATED lConfidence "Use generic-lens or generic-optics with 'confidence' instead." #-}

-- | The parent labels for a label. The response includes all ancestor labels.
--
-- /Note:/ Consider using 'parents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lParents :: Lens.Lens' Label (Lude.Maybe [Parent])
lParents = Lens.lens (parents :: Label -> Lude.Maybe [Parent]) (\s a -> s {parents = a} :: Label)
{-# DEPRECATED lParents "Use generic-lens or generic-optics with 'parents' instead." #-}

-- | The name (label) of the object or scene.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lName :: Lens.Lens' Label (Lude.Maybe Lude.Text)
lName = Lens.lens (name :: Label -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Label)
{-# DEPRECATED lName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If @Label@ represents an object, @Instances@ contains the bounding boxes for each instance of the detected object. Bounding boxes are returned for common object labels such as people, cars, furniture, apparel or pets.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lInstances :: Lens.Lens' Label (Lude.Maybe [Instance])
lInstances = Lens.lens (instances :: Label -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: Label)
{-# DEPRECATED lInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

instance Lude.FromJSON Label where
  parseJSON =
    Lude.withObject
      "Label"
      ( \x ->
          Label'
            Lude.<$> (x Lude..:? "Confidence")
            Lude.<*> (x Lude..:? "Parents" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Instances" Lude..!= Lude.mempty)
      )
