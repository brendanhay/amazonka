{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.OpsEntity
  ( OpsEntity (..)
  -- * Smart constructor
  , mkOpsEntity
  -- * Lenses
  , oeData
  , oeId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.OpsEntityId as Types
import qualified Network.AWS.SSM.Types.OpsEntityItem as Types
import qualified Network.AWS.SSM.Types.OpsEntityItemKey as Types

-- | The result of the query.
--
-- /See:/ 'mkOpsEntity' smart constructor.
data OpsEntity = OpsEntity'
  { data' :: Core.Maybe (Core.HashMap Types.OpsEntityItemKey Types.OpsEntityItem)
    -- ^ The data returned by the query.
  , id :: Core.Maybe Types.OpsEntityId
    -- ^ The query ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OpsEntity' value with any optional fields omitted.
mkOpsEntity
    :: OpsEntity
mkOpsEntity = OpsEntity'{data' = Core.Nothing, id = Core.Nothing}

-- | The data returned by the query.
--
-- /Note:/ Consider using 'data'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeData :: Lens.Lens' OpsEntity (Core.Maybe (Core.HashMap Types.OpsEntityItemKey Types.OpsEntityItem))
oeData = Lens.field @"data'"
{-# INLINEABLE oeData #-}
{-# DEPRECATED data' "Use generic-lens or generic-optics with 'data'' instead"  #-}

-- | The query ID.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeId :: Lens.Lens' OpsEntity (Core.Maybe Types.OpsEntityId)
oeId = Lens.field @"id"
{-# INLINEABLE oeId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

instance Core.FromJSON OpsEntity where
        parseJSON
          = Core.withObject "OpsEntity" Core.$
              \ x ->
                OpsEntity' Core.<$> (x Core..:? "Data") Core.<*> x Core..:? "Id"
