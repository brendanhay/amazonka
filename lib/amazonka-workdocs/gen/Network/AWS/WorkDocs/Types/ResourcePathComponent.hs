{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ResourcePathComponent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.ResourcePathComponent
  ( ResourcePathComponent (..)
  -- * Smart constructor
  , mkResourcePathComponent
  -- * Lenses
  , rpcId
  , rpcName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.IdType as Types
import qualified Network.AWS.WorkDocs.Types.Name as Types

-- | Describes the resource path.
--
-- /See:/ 'mkResourcePathComponent' smart constructor.
data ResourcePathComponent = ResourcePathComponent'
  { id :: Core.Maybe Types.IdType
    -- ^ The ID of the resource path.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the resource path.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResourcePathComponent' value with any optional fields omitted.
mkResourcePathComponent
    :: ResourcePathComponent
mkResourcePathComponent
  = ResourcePathComponent'{id = Core.Nothing, name = Core.Nothing}

-- | The ID of the resource path.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpcId :: Lens.Lens' ResourcePathComponent (Core.Maybe Types.IdType)
rpcId = Lens.field @"id"
{-# INLINEABLE rpcId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the resource path.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpcName :: Lens.Lens' ResourcePathComponent (Core.Maybe Types.Name)
rpcName = Lens.field @"name"
{-# INLINEABLE rpcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON ResourcePathComponent where
        parseJSON
          = Core.withObject "ResourcePathComponent" Core.$
              \ x ->
                ResourcePathComponent' Core.<$>
                  (x Core..:? "Id") Core.<*> x Core..:? "Name"
