{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.AddAttributesActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.AddAttributesActivity
  ( AddAttributesActivity (..),

    -- * Smart constructor
    mkAddAttributesActivity,

    -- * Lenses
    aaaNext,
    aaaName,
    aaaAttributes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An activity that adds other attributes based on existing attributes in the message.
--
-- /See:/ 'mkAddAttributesActivity' smart constructor.
data AddAttributesActivity = AddAttributesActivity'
  { next ::
      Lude.Maybe Lude.Text,
    name :: Lude.Text,
    attributes ::
      Lude.HashMap Lude.Text (Lude.Text)
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddAttributesActivity' with the minimum fields required to make a request.
--
-- * 'attributes' - A list of 1-50 @AttributeNameMapping@ objects that map an existing attribute to a new attribute.
-- * 'name' - The name of the addAttributes activity.
-- * 'next' - The next activity in the pipeline.
mkAddAttributesActivity ::
  -- | 'name'
  Lude.Text ->
  AddAttributesActivity
mkAddAttributesActivity pName_ =
  AddAttributesActivity'
    { next = Lude.Nothing,
      name = pName_,
      attributes = Lude.mempty
    }

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaaNext :: Lens.Lens' AddAttributesActivity (Lude.Maybe Lude.Text)
aaaNext = Lens.lens (next :: AddAttributesActivity -> Lude.Maybe Lude.Text) (\s a -> s {next = a} :: AddAttributesActivity)
{-# DEPRECATED aaaNext "Use generic-lens or generic-optics with 'next' instead." #-}

-- | The name of the addAttributes activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaaName :: Lens.Lens' AddAttributesActivity Lude.Text
aaaName = Lens.lens (name :: AddAttributesActivity -> Lude.Text) (\s a -> s {name = a} :: AddAttributesActivity)
{-# DEPRECATED aaaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of 1-50 @AttributeNameMapping@ objects that map an existing attribute to a new attribute.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaaAttributes :: Lens.Lens' AddAttributesActivity (Lude.HashMap Lude.Text (Lude.Text))
aaaAttributes = Lens.lens (attributes :: AddAttributesActivity -> Lude.HashMap Lude.Text (Lude.Text)) (\s a -> s {attributes = a} :: AddAttributesActivity)
{-# DEPRECATED aaaAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.FromJSON AddAttributesActivity where
  parseJSON =
    Lude.withObject
      "AddAttributesActivity"
      ( \x ->
          AddAttributesActivity'
            Lude.<$> (x Lude..:? "next")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON AddAttributesActivity where
  toJSON AddAttributesActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("next" Lude..=) Lude.<$> next,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("attributes" Lude..= attributes)
          ]
      )
