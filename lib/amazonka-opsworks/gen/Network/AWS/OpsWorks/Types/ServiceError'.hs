{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.ServiceError'
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.ServiceError'
  ( ServiceError' (..)
  -- * Smart constructor
  , mkServiceError'
  -- * Lenses
  , seCreatedAt
  , seInstanceId
  , seMessage
  , seServiceErrorId
  , seStackId
  , seType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.OpsWorks.Types.CreatedAt as Types
import qualified Network.AWS.Prelude as Core

-- | Describes an AWS OpsWorks Stacks service error.
--
-- /See:/ 'mkServiceError'' smart constructor.
data ServiceError' = ServiceError''
  { createdAt :: Core.Maybe Types.CreatedAt
    -- ^ When the error occurred.
  , instanceId :: Core.Maybe Core.Text
    -- ^ The instance ID.
  , message :: Core.Maybe Core.Text
    -- ^ A message that describes the error.
  , serviceErrorId :: Core.Maybe Core.Text
    -- ^ The error ID.
  , stackId :: Core.Maybe Core.Text
    -- ^ The stack ID.
  , type' :: Core.Maybe Core.Text
    -- ^ The error type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServiceError'' value with any optional fields omitted.
mkServiceError'
    :: ServiceError'
mkServiceError'
  = ServiceError''{createdAt = Core.Nothing,
                   instanceId = Core.Nothing, message = Core.Nothing,
                   serviceErrorId = Core.Nothing, stackId = Core.Nothing,
                   type' = Core.Nothing}

-- | When the error occurred.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seCreatedAt :: Lens.Lens' ServiceError' (Core.Maybe Types.CreatedAt)
seCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE seCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The instance ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seInstanceId :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
seInstanceId = Lens.field @"instanceId"
{-# INLINEABLE seInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | A message that describes the error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMessage :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
seMessage = Lens.field @"message"
{-# INLINEABLE seMessage #-}
{-# DEPRECATED message "Use generic-lens or generic-optics with 'message' instead"  #-}

-- | The error ID.
--
-- /Note:/ Consider using 'serviceErrorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seServiceErrorId :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
seServiceErrorId = Lens.field @"serviceErrorId"
{-# INLINEABLE seServiceErrorId #-}
{-# DEPRECATED serviceErrorId "Use generic-lens or generic-optics with 'serviceErrorId' instead"  #-}

-- | The stack ID.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seStackId :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
seStackId = Lens.field @"stackId"
{-# INLINEABLE seStackId #-}
{-# DEPRECATED stackId "Use generic-lens or generic-optics with 'stackId' instead"  #-}

-- | The error type.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seType :: Lens.Lens' ServiceError' (Core.Maybe Core.Text)
seType = Lens.field @"type'"
{-# INLINEABLE seType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON ServiceError' where
        parseJSON
          = Core.withObject "ServiceError'" Core.$
              \ x ->
                ServiceError'' Core.<$>
                  (x Core..:? "CreatedAt") Core.<*> x Core..:? "InstanceId" Core.<*>
                    x Core..:? "Message"
                    Core.<*> x Core..:? "ServiceErrorId"
                    Core.<*> x Core..:? "StackId"
                    Core.<*> x Core..:? "Type"
