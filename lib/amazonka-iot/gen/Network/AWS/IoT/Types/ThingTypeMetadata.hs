{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ThingTypeMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeMetadata
  ( ThingTypeMetadata (..),

    -- * Smart constructor
    mkThingTypeMetadata,

    -- * Lenses
    ttmDeprecationDate,
    ttmCreationDate,
    ttmDeprecated,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when time was deprecated.
--
-- /See:/ 'mkThingTypeMetadata' smart constructor.
data ThingTypeMetadata = ThingTypeMetadata'
  { -- | The date and time when the thing type was deprecated.
    deprecationDate :: Lude.Maybe Lude.Timestamp,
    -- | The date and time when the thing type was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | Whether the thing type is deprecated. If __true__ , no new things could be associated with this type.
    deprecated :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ThingTypeMetadata' with the minimum fields required to make a request.
--
-- * 'deprecationDate' - The date and time when the thing type was deprecated.
-- * 'creationDate' - The date and time when the thing type was created.
-- * 'deprecated' - Whether the thing type is deprecated. If __true__ , no new things could be associated with this type.
mkThingTypeMetadata ::
  ThingTypeMetadata
mkThingTypeMetadata =
  ThingTypeMetadata'
    { deprecationDate = Lude.Nothing,
      creationDate = Lude.Nothing,
      deprecated = Lude.Nothing
    }

-- | The date and time when the thing type was deprecated.
--
-- /Note:/ Consider using 'deprecationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttmDeprecationDate :: Lens.Lens' ThingTypeMetadata (Lude.Maybe Lude.Timestamp)
ttmDeprecationDate = Lens.lens (deprecationDate :: ThingTypeMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {deprecationDate = a} :: ThingTypeMetadata)
{-# DEPRECATED ttmDeprecationDate "Use generic-lens or generic-optics with 'deprecationDate' instead." #-}

-- | The date and time when the thing type was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttmCreationDate :: Lens.Lens' ThingTypeMetadata (Lude.Maybe Lude.Timestamp)
ttmCreationDate = Lens.lens (creationDate :: ThingTypeMetadata -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: ThingTypeMetadata)
{-# DEPRECATED ttmCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Whether the thing type is deprecated. If __true__ , no new things could be associated with this type.
--
-- /Note:/ Consider using 'deprecated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ttmDeprecated :: Lens.Lens' ThingTypeMetadata (Lude.Maybe Lude.Bool)
ttmDeprecated = Lens.lens (deprecated :: ThingTypeMetadata -> Lude.Maybe Lude.Bool) (\s a -> s {deprecated = a} :: ThingTypeMetadata)
{-# DEPRECATED ttmDeprecated "Use generic-lens or generic-optics with 'deprecated' instead." #-}

instance Lude.FromJSON ThingTypeMetadata where
  parseJSON =
    Lude.withObject
      "ThingTypeMetadata"
      ( \x ->
          ThingTypeMetadata'
            Lude.<$> (x Lude..:? "deprecationDate")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "deprecated")
      )
