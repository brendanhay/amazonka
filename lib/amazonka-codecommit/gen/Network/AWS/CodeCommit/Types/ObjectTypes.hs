-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ObjectTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ObjectTypes
  ( ObjectTypes (..),

    -- * Smart constructor
    mkObjectTypes,

    -- * Lenses
    otDestination,
    otBase,
    otSource,
  )
where

import Network.AWS.CodeCommit.Types.ObjectTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the type of an object in a merge operation.
--
-- /See:/ 'mkObjectTypes' smart constructor.
data ObjectTypes = ObjectTypes'
  { destination ::
      Lude.Maybe ObjectTypeEnum,
    base :: Lude.Maybe ObjectTypeEnum,
    source :: Lude.Maybe ObjectTypeEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectTypes' with the minimum fields required to make a request.
--
-- * 'base' - The type of the object in the base commit of the merge.
-- * 'destination' - The type of the object in the destination branch.
-- * 'source' - The type of the object in the source branch.
mkObjectTypes ::
  ObjectTypes
mkObjectTypes =
  ObjectTypes'
    { destination = Lude.Nothing,
      base = Lude.Nothing,
      source = Lude.Nothing
    }

-- | The type of the object in the destination branch.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otDestination :: Lens.Lens' ObjectTypes (Lude.Maybe ObjectTypeEnum)
otDestination = Lens.lens (destination :: ObjectTypes -> Lude.Maybe ObjectTypeEnum) (\s a -> s {destination = a} :: ObjectTypes)
{-# DEPRECATED otDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The type of the object in the base commit of the merge.
--
-- /Note:/ Consider using 'base' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otBase :: Lens.Lens' ObjectTypes (Lude.Maybe ObjectTypeEnum)
otBase = Lens.lens (base :: ObjectTypes -> Lude.Maybe ObjectTypeEnum) (\s a -> s {base = a} :: ObjectTypes)
{-# DEPRECATED otBase "Use generic-lens or generic-optics with 'base' instead." #-}

-- | The type of the object in the source branch.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otSource :: Lens.Lens' ObjectTypes (Lude.Maybe ObjectTypeEnum)
otSource = Lens.lens (source :: ObjectTypes -> Lude.Maybe ObjectTypeEnum) (\s a -> s {source = a} :: ObjectTypes)
{-# DEPRECATED otSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Lude.FromJSON ObjectTypes where
  parseJSON =
    Lude.withObject
      "ObjectTypes"
      ( \x ->
          ObjectTypes'
            Lude.<$> (x Lude..:? "destination")
            Lude.<*> (x Lude..:? "base")
            Lude.<*> (x Lude..:? "source")
      )
