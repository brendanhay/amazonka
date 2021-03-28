{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.GroupMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.WorkDocs.Types.GroupMetadata
  ( GroupMetadata (..)
  -- * Smart constructor
  , mkGroupMetadata
  -- * Lenses
  , gmId
  , gmName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.GroupNameType as Types
import qualified Network.AWS.WorkDocs.Types.IdType as Types

-- | Describes the metadata of a user group.
--
-- /See:/ 'mkGroupMetadata' smart constructor.
data GroupMetadata = GroupMetadata'
  { id :: Core.Maybe Types.IdType
    -- ^ The ID of the user group.
  , name :: Core.Maybe Types.GroupNameType
    -- ^ The name of the group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GroupMetadata' value with any optional fields omitted.
mkGroupMetadata
    :: GroupMetadata
mkGroupMetadata
  = GroupMetadata'{id = Core.Nothing, name = Core.Nothing}

-- | The ID of the user group.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmId :: Lens.Lens' GroupMetadata (Core.Maybe Types.IdType)
gmId = Lens.field @"id"
{-# INLINEABLE gmId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The name of the group.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmName :: Lens.Lens' GroupMetadata (Core.Maybe Types.GroupNameType)
gmName = Lens.field @"name"
{-# INLINEABLE gmName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.FromJSON GroupMetadata where
        parseJSON
          = Core.withObject "GroupMetadata" Core.$
              \ x ->
                GroupMetadata' Core.<$>
                  (x Core..:? "Id") Core.<*> x Core..:? "Name"
