-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingTypeProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeProperties
  ( ThingTypeProperties (..),

    -- * Smart constructor
    mkThingTypeProperties,

    -- * Lenses
    ttpSearchableAttributes,
    ttpThingTypeDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The ThingTypeProperties contains information about the thing type including: a thing type description, and a list of searchable thing attribute names.
--
-- /See:/ 'mkThingTypeProperties' smart constructor.
data ThingTypeProperties = ThingTypeProperties'
  { searchableAttributes ::
      Lude.Maybe [Lude.Text],
    thingTypeDescription :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingTypeProperties' with the minimum fields required to make a request.
--
-- * 'searchableAttributes' - A list of searchable thing attribute names.
-- * 'thingTypeDescription' - The description of the thing type.
mkThingTypeProperties ::
  ThingTypeProperties
mkThingTypeProperties =
  ThingTypeProperties'
    { searchableAttributes = Lude.Nothing,
      thingTypeDescription = Lude.Nothing
    }

-- | A list of searchable thing attribute names.
--
-- /Note:/ Consider using 'searchableAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttpSearchableAttributes :: Lens.Lens' ThingTypeProperties (Lude.Maybe [Lude.Text])
ttpSearchableAttributes = Lens.lens (searchableAttributes :: ThingTypeProperties -> Lude.Maybe [Lude.Text]) (\s a -> s {searchableAttributes = a} :: ThingTypeProperties)
{-# DEPRECATED ttpSearchableAttributes "Use generic-lens or generic-optics with 'searchableAttributes' instead." #-}

-- | The description of the thing type.
--
-- /Note:/ Consider using 'thingTypeDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttpThingTypeDescription :: Lens.Lens' ThingTypeProperties (Lude.Maybe Lude.Text)
ttpThingTypeDescription = Lens.lens (thingTypeDescription :: ThingTypeProperties -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeDescription = a} :: ThingTypeProperties)
{-# DEPRECATED ttpThingTypeDescription "Use generic-lens or generic-optics with 'thingTypeDescription' instead." #-}

instance Lude.FromJSON ThingTypeProperties where
  parseJSON =
    Lude.withObject
      "ThingTypeProperties"
      ( \x ->
          ThingTypeProperties'
            Lude.<$> (x Lude..:? "searchableAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "thingTypeDescription")
      )

instance Lude.ToJSON ThingTypeProperties where
  toJSON ThingTypeProperties' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("searchableAttributes" Lude..=) Lude.<$> searchableAttributes,
            ("thingTypeDescription" Lude..=) Lude.<$> thingTypeDescription
          ]
      )
