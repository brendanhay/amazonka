{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Delete
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Delete
  ( Delete (..)
  -- * Smart constructor
  , mkDelete
  -- * Lenses
  , dObjects
  , dQuiet
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.ObjectIdentifier as Types

-- | Container for the objects to delete.
--
-- /See:/ 'mkDelete' smart constructor.
data Delete = Delete'
  { objects :: [Types.ObjectIdentifier]
    -- ^ The objects to delete.
  , quiet :: Core.Maybe Core.Bool
    -- ^ Element to enable quiet mode for the request. When you add this element, you must set its value to true.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Delete' value with any optional fields omitted.
mkDelete
    :: Delete
mkDelete = Delete'{objects = Core.mempty, quiet = Core.Nothing}

-- | The objects to delete.
--
-- /Note:/ Consider using 'objects' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dObjects :: Lens.Lens' Delete [Types.ObjectIdentifier]
dObjects = Lens.field @"objects"
{-# INLINEABLE dObjects #-}
{-# DEPRECATED objects "Use generic-lens or generic-optics with 'objects' instead"  #-}

-- | Element to enable quiet mode for the request. When you add this element, you must set its value to true.
--
-- /Note:/ Consider using 'quiet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dQuiet :: Lens.Lens' Delete (Core.Maybe Core.Bool)
dQuiet = Lens.field @"quiet"
{-# INLINEABLE dQuiet #-}
{-# DEPRECATED quiet "Use generic-lens or generic-optics with 'quiet' instead"  #-}

instance Core.ToXML Delete where
        toXML Delete{..}
          = Core.toXMLList "Object" objects Core.<>
              Core.maybe Core.mempty (Core.toXMLElement "Quiet") quiet
