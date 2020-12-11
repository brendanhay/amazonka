-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingTypeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeDefinition
  ( ThingTypeDefinition (..),

    -- * Smart constructor
    mkThingTypeDefinition,

    -- * Lenses
    ttdThingTypeProperties,
    ttdThingTypeName,
    ttdThingTypeMetadata,
    ttdThingTypeARN,
  )
where

import Network.AWS.IoT.Types.ThingTypeMetadata
import Network.AWS.IoT.Types.ThingTypeProperties
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The definition of the thing type, including thing type name and description.
--
-- /See:/ 'mkThingTypeDefinition' smart constructor.
data ThingTypeDefinition = ThingTypeDefinition'
  { thingTypeProperties ::
      Lude.Maybe ThingTypeProperties,
    thingTypeName :: Lude.Maybe Lude.Text,
    thingTypeMetadata :: Lude.Maybe ThingTypeMetadata,
    thingTypeARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingTypeDefinition' with the minimum fields required to make a request.
--
-- * 'thingTypeARN' - The thing type ARN.
-- * 'thingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
-- * 'thingTypeName' - The name of the thing type.
-- * 'thingTypeProperties' - The ThingTypeProperties for the thing type.
mkThingTypeDefinition ::
  ThingTypeDefinition
mkThingTypeDefinition =
  ThingTypeDefinition'
    { thingTypeProperties = Lude.Nothing,
      thingTypeName = Lude.Nothing,
      thingTypeMetadata = Lude.Nothing,
      thingTypeARN = Lude.Nothing
    }

-- | The ThingTypeProperties for the thing type.
--
-- /Note:/ Consider using 'thingTypeProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttdThingTypeProperties :: Lens.Lens' ThingTypeDefinition (Lude.Maybe ThingTypeProperties)
ttdThingTypeProperties = Lens.lens (thingTypeProperties :: ThingTypeDefinition -> Lude.Maybe ThingTypeProperties) (\s a -> s {thingTypeProperties = a} :: ThingTypeDefinition)
{-# DEPRECATED ttdThingTypeProperties "Use generic-lens or generic-optics with 'thingTypeProperties' instead." #-}

-- | The name of the thing type.
--
-- /Note:/ Consider using 'thingTypeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttdThingTypeName :: Lens.Lens' ThingTypeDefinition (Lude.Maybe Lude.Text)
ttdThingTypeName = Lens.lens (thingTypeName :: ThingTypeDefinition -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeName = a} :: ThingTypeDefinition)
{-# DEPRECATED ttdThingTypeName "Use generic-lens or generic-optics with 'thingTypeName' instead." #-}

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
--
-- /Note:/ Consider using 'thingTypeMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttdThingTypeMetadata :: Lens.Lens' ThingTypeDefinition (Lude.Maybe ThingTypeMetadata)
ttdThingTypeMetadata = Lens.lens (thingTypeMetadata :: ThingTypeDefinition -> Lude.Maybe ThingTypeMetadata) (\s a -> s {thingTypeMetadata = a} :: ThingTypeDefinition)
{-# DEPRECATED ttdThingTypeMetadata "Use generic-lens or generic-optics with 'thingTypeMetadata' instead." #-}

-- | The thing type ARN.
--
-- /Note:/ Consider using 'thingTypeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttdThingTypeARN :: Lens.Lens' ThingTypeDefinition (Lude.Maybe Lude.Text)
ttdThingTypeARN = Lens.lens (thingTypeARN :: ThingTypeDefinition -> Lude.Maybe Lude.Text) (\s a -> s {thingTypeARN = a} :: ThingTypeDefinition)
{-# DEPRECATED ttdThingTypeARN "Use generic-lens or generic-optics with 'thingTypeARN' instead." #-}

instance Lude.FromJSON ThingTypeDefinition where
  parseJSON =
    Lude.withObject
      "ThingTypeDefinition"
      ( \x ->
          ThingTypeDefinition'
            Lude.<$> (x Lude..:? "thingTypeProperties")
            Lude.<*> (x Lude..:? "thingTypeName")
            Lude.<*> (x Lude..:? "thingTypeMetadata")
            Lude.<*> (x Lude..:? "thingTypeArn")
      )
