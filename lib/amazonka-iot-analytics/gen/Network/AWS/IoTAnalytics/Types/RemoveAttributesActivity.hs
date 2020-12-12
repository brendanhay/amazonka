{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.RemoveAttributesActivity
  ( RemoveAttributesActivity (..),

    -- * Smart constructor
    mkRemoveAttributesActivity,

    -- * Lenses
    raaNext,
    raaName,
    raaAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An activity that removes attributes from a message.
--
-- /See:/ 'mkRemoveAttributesActivity' smart constructor.
data RemoveAttributesActivity = RemoveAttributesActivity'
  { next ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text,
    attributes :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveAttributesActivity' with the minimum fields required to make a request.
--
-- * 'attributes' - A list of 1-50 attributes to remove from the message.
-- * 'name' - The name of the @removeAttributes@ activity.
-- * 'next' - The next activity in the pipeline.
mkRemoveAttributesActivity ::
  -- | 'name'
  Lude.Text ->
  -- | 'attributes'
  Lude.NonEmpty Lude.Text ->
  RemoveAttributesActivity
mkRemoveAttributesActivity pName_ pAttributes_ =
  RemoveAttributesActivity'
    { next = Lude.Nothing,
      name = pName_,
      attributes = pAttributes_
    }

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raaNext :: Lens.Lens' RemoveAttributesActivity (Lude.Maybe Lude.Text)
raaNext = Lens.lens (next :: RemoveAttributesActivity -> Lude.Maybe Lude.Text) (\s a -> s {next = a} :: RemoveAttributesActivity)
{-# DEPRECATED raaNext "Use generic-lens or generic-optics with 'next' instead." #-}

-- | The name of the @removeAttributes@ activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raaName :: Lens.Lens' RemoveAttributesActivity Lude.Text
raaName = Lens.lens (name :: RemoveAttributesActivity -> Lude.Text) (\s a -> s {name = a} :: RemoveAttributesActivity)
{-# DEPRECATED raaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of 1-50 attributes to remove from the message.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raaAttributes :: Lens.Lens' RemoveAttributesActivity (Lude.NonEmpty Lude.Text)
raaAttributes = Lens.lens (attributes :: RemoveAttributesActivity -> Lude.NonEmpty Lude.Text) (\s a -> s {attributes = a} :: RemoveAttributesActivity)
{-# DEPRECATED raaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON RemoveAttributesActivity where
  parseJSON =
    Lude.withObject
      "RemoveAttributesActivity"
      ( \x ->
          RemoveAttributesActivity'
            Lude.<$> (x Lude..:? "next")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "attributes")
      )

instance Lude.ToJSON RemoveAttributesActivity where
  toJSON RemoveAttributesActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("next" Lude..=) Lude.<$> next,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("attributes" Lude..= attributes)
          ]
      )
