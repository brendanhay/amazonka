{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ComputeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkSpaces.Types.ComputeType
  ( ComputeType (..)
  -- * Smart constructor
  , mkComputeType
  -- * Lenses
  , ctName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkSpaces.Types.Compute as Types

-- | Describes the compute type.
--
-- /See:/ 'mkComputeType' smart constructor.
newtype ComputeType = ComputeType'
  { name :: Core.Maybe Types.Compute
    -- ^ The compute type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ComputeType' value with any optional fields omitted.
mkComputeType
    :: ComputeType
mkComputeType = ComputeType'{name = Core.Nothing}

-- | The compute type.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctName :: Lens.Lens' ComputeType (Core.Maybe Types.Compute)
ctName = Lens.field @"name"
{-# INLINEABLE ctName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON ComputeType where
        parseJSON
          = Core.withObject "ComputeType" Core.$
              \ x -> ComputeType' Core.<$> (x Core..:? "Name")
