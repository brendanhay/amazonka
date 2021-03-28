{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Parent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.Parent
  ( Parent (..)
  -- * Smart constructor
  , mkParent
  -- * Lenses
  , pName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A parent label for a label. A label can have 0, 1, or more parents. 
--
-- /See:/ 'mkParent' smart constructor.
newtype Parent = Parent'
  { name :: Core.Maybe Core.Text
    -- ^ The name of the parent label.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Parent' value with any optional fields omitted.
mkParent
    :: Parent
mkParent = Parent'{name = Core.Nothing}

-- | The name of the parent label.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pName :: Lens.Lens' Parent (Core.Maybe Core.Text)
pName = Lens.field @"name"
{-# INLINEABLE pName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON Parent where
        parseJSON
          = Core.withObject "Parent" Core.$
              \ x -> Parent' Core.<$> (x Core..:? "Name")
