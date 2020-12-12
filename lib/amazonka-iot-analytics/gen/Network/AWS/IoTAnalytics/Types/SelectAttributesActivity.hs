{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.SelectAttributesActivity
  ( SelectAttributesActivity (..),

    -- * Smart constructor
    mkSelectAttributesActivity,

    -- * Lenses
    saaNext,
    saaName,
    saaAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Creates a new message using only the specified attributes from the original message.
--
-- /See:/ 'mkSelectAttributesActivity' smart constructor.
data SelectAttributesActivity = SelectAttributesActivity'
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

-- | Creates a value of 'SelectAttributesActivity' with the minimum fields required to make a request.
--
-- * 'attributes' - A list of the attributes to select from the message.
-- * 'name' - The name of the @selectAttributes@ activity.
-- * 'next' - The next activity in the pipeline.
mkSelectAttributesActivity ::
  -- | 'name'
  Lude.Text ->
  -- | 'attributes'
  Lude.NonEmpty Lude.Text ->
  SelectAttributesActivity
mkSelectAttributesActivity pName_ pAttributes_ =
  SelectAttributesActivity'
    { next = Lude.Nothing,
      name = pName_,
      attributes = pAttributes_
    }

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaNext :: Lens.Lens' SelectAttributesActivity (Lude.Maybe Lude.Text)
saaNext = Lens.lens (next :: SelectAttributesActivity -> Lude.Maybe Lude.Text) (\s a -> s {next = a} :: SelectAttributesActivity)
{-# DEPRECATED saaNext "Use generic-lens or generic-optics with 'next' instead." #-}

-- | The name of the @selectAttributes@ activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaName :: Lens.Lens' SelectAttributesActivity Lude.Text
saaName = Lens.lens (name :: SelectAttributesActivity -> Lude.Text) (\s a -> s {name = a} :: SelectAttributesActivity)
{-# DEPRECATED saaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of the attributes to select from the message.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saaAttributes :: Lens.Lens' SelectAttributesActivity (Lude.NonEmpty Lude.Text)
saaAttributes = Lens.lens (attributes :: SelectAttributesActivity -> Lude.NonEmpty Lude.Text) (\s a -> s {attributes = a} :: SelectAttributesActivity)
{-# DEPRECATED saaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON SelectAttributesActivity where
  parseJSON =
    Lude.withObject
      "SelectAttributesActivity"
      ( \x ->
          SelectAttributesActivity'
            Lude.<$> (x Lude..:? "next")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "attributes")
      )

instance Lude.ToJSON SelectAttributesActivity where
  toJSON SelectAttributesActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("next" Lude..=) Lude.<$> next,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("attributes" Lude..= attributes)
          ]
      )
