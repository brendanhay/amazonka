{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingGroupProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupProperties
  ( ThingGroupProperties (..),

    -- * Smart constructor
    mkThingGroupProperties,

    -- * Lenses
    tgpAttributePayload,
    tgpThingGroupDescription,
  )
where

import Network.AWS.IoT.Types.AttributePayload
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Thing group properties.
--
-- /See:/ 'mkThingGroupProperties' smart constructor.
data ThingGroupProperties = ThingGroupProperties'
  { attributePayload ::
      Lude.Maybe AttributePayload,
    thingGroupDescription :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingGroupProperties' with the minimum fields required to make a request.
--
-- * 'attributePayload' - The thing group attributes in JSON format.
-- * 'thingGroupDescription' - The thing group description.
mkThingGroupProperties ::
  ThingGroupProperties
mkThingGroupProperties =
  ThingGroupProperties'
    { attributePayload = Lude.Nothing,
      thingGroupDescription = Lude.Nothing
    }

-- | The thing group attributes in JSON format.
--
-- /Note:/ Consider using 'attributePayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpAttributePayload :: Lens.Lens' ThingGroupProperties (Lude.Maybe AttributePayload)
tgpAttributePayload = Lens.lens (attributePayload :: ThingGroupProperties -> Lude.Maybe AttributePayload) (\s a -> s {attributePayload = a} :: ThingGroupProperties)
{-# DEPRECATED tgpAttributePayload "Use generic-lens or generic-optics with 'attributePayload' instead." #-}

-- | The thing group description.
--
-- /Note:/ Consider using 'thingGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgpThingGroupDescription :: Lens.Lens' ThingGroupProperties (Lude.Maybe Lude.Text)
tgpThingGroupDescription = Lens.lens (thingGroupDescription :: ThingGroupProperties -> Lude.Maybe Lude.Text) (\s a -> s {thingGroupDescription = a} :: ThingGroupProperties)
{-# DEPRECATED tgpThingGroupDescription "Use generic-lens or generic-optics with 'thingGroupDescription' instead." #-}

instance Lude.FromJSON ThingGroupProperties where
  parseJSON =
    Lude.withObject
      "ThingGroupProperties"
      ( \x ->
          ThingGroupProperties'
            Lude.<$> (x Lude..:? "attributePayload")
            Lude.<*> (x Lude..:? "thingGroupDescription")
      )

instance Lude.ToJSON ThingGroupProperties where
  toJSON ThingGroupProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("attributePayload" Lude..=) Lude.<$> attributePayload,
            ("thingGroupDescription" Lude..=) Lude.<$> thingGroupDescription
          ]
      )
