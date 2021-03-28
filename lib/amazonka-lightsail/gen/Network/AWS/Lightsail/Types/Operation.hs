{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Operation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.Operation
  ( Operation (..)
  -- * Smart constructor
  , mkOperation
  -- * Lenses
  , ofCreatedAt
  , ofErrorCode
  , ofErrorDetails
  , ofId
  , ofIsTerminal
  , ofLocation
  , ofOperationDetails
  , ofOperationType
  , ofResourceName
  , ofResourceType
  , ofStatus
  , ofStatusChangedAt
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Id as Types
import qualified Network.AWS.Lightsail.Types.OperationStatus as Types
import qualified Network.AWS.Lightsail.Types.OperationType as Types
import qualified Network.AWS.Lightsail.Types.ResourceLocation as Types
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Lightsail.Types.ResourceType as Types
import qualified Network.AWS.Prelude as Core

-- | Describes the API operation.
--
-- /See:/ 'mkOperation' smart constructor.
data Operation = Operation'
  { createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
  , errorCode :: Core.Maybe Core.Text
    -- ^ The error code.
  , errorDetails :: Core.Maybe Core.Text
    -- ^ The error details.
  , id :: Core.Maybe Types.Id
    -- ^ The ID of the operation.
  , isTerminal :: Core.Maybe Core.Bool
    -- ^ A Boolean value indicating whether the operation is terminal.
  , location :: Core.Maybe Types.ResourceLocation
    -- ^ The AWS Region and Availability Zone.
  , operationDetails :: Core.Maybe Core.Text
    -- ^ Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
  , operationType :: Core.Maybe Types.OperationType
    -- ^ The type of operation. 
  , resourceName :: Core.Maybe Types.ResourceName
    -- ^ The resource name.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The resource type. 
  , status :: Core.Maybe Types.OperationStatus
    -- ^ The status of the operation. 
  , statusChangedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when the status was changed (e.g., @1479816991.349@ ).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Operation' value with any optional fields omitted.
mkOperation
    :: Operation
mkOperation
  = Operation'{createdAt = Core.Nothing, errorCode = Core.Nothing,
               errorDetails = Core.Nothing, id = Core.Nothing,
               isTerminal = Core.Nothing, location = Core.Nothing,
               operationDetails = Core.Nothing, operationType = Core.Nothing,
               resourceName = Core.Nothing, resourceType = Core.Nothing,
               status = Core.Nothing, statusChangedAt = Core.Nothing}

-- | The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofCreatedAt :: Lens.Lens' Operation (Core.Maybe Core.NominalDiffTime)
ofCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE ofCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofErrorCode :: Lens.Lens' Operation (Core.Maybe Core.Text)
ofErrorCode = Lens.field @"errorCode"
{-# INLINEABLE ofErrorCode #-}
{-# DEPRECATED errorCode "Use generic-lens or generic-optics with 'errorCode' instead"  #-}

-- | The error details.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofErrorDetails :: Lens.Lens' Operation (Core.Maybe Core.Text)
ofErrorDetails = Lens.field @"errorDetails"
{-# INLINEABLE ofErrorDetails #-}
{-# DEPRECATED errorDetails "Use generic-lens or generic-optics with 'errorDetails' instead"  #-}

-- | The ID of the operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofId :: Lens.Lens' Operation (Core.Maybe Types.Id)
ofId = Lens.field @"id"
{-# INLINEABLE ofId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | A Boolean value indicating whether the operation is terminal.
--
-- /Note:/ Consider using 'isTerminal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofIsTerminal :: Lens.Lens' Operation (Core.Maybe Core.Bool)
ofIsTerminal = Lens.field @"isTerminal"
{-# INLINEABLE ofIsTerminal #-}
{-# DEPRECATED isTerminal "Use generic-lens or generic-optics with 'isTerminal' instead"  #-}

-- | The AWS Region and Availability Zone.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofLocation :: Lens.Lens' Operation (Core.Maybe Types.ResourceLocation)
ofLocation = Lens.field @"location"
{-# INLINEABLE ofLocation #-}
{-# DEPRECATED location "Use generic-lens or generic-optics with 'location' instead"  #-}

-- | Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
--
-- /Note:/ Consider using 'operationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofOperationDetails :: Lens.Lens' Operation (Core.Maybe Core.Text)
ofOperationDetails = Lens.field @"operationDetails"
{-# INLINEABLE ofOperationDetails #-}
{-# DEPRECATED operationDetails "Use generic-lens or generic-optics with 'operationDetails' instead"  #-}

-- | The type of operation. 
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofOperationType :: Lens.Lens' Operation (Core.Maybe Types.OperationType)
ofOperationType = Lens.field @"operationType"
{-# INLINEABLE ofOperationType #-}
{-# DEPRECATED operationType "Use generic-lens or generic-optics with 'operationType' instead"  #-}

-- | The resource name.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofResourceName :: Lens.Lens' Operation (Core.Maybe Types.ResourceName)
ofResourceName = Lens.field @"resourceName"
{-# INLINEABLE ofResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

-- | The resource type. 
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofResourceType :: Lens.Lens' Operation (Core.Maybe Types.ResourceType)
ofResourceType = Lens.field @"resourceType"
{-# INLINEABLE ofResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The status of the operation. 
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofStatus :: Lens.Lens' Operation (Core.Maybe Types.OperationStatus)
ofStatus = Lens.field @"status"
{-# INLINEABLE ofStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The timestamp when the status was changed (e.g., @1479816991.349@ ).
--
-- /Note:/ Consider using 'statusChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ofStatusChangedAt :: Lens.Lens' Operation (Core.Maybe Core.NominalDiffTime)
ofStatusChangedAt = Lens.field @"statusChangedAt"
{-# INLINEABLE ofStatusChangedAt #-}
{-# DEPRECATED statusChangedAt "Use generic-lens or generic-optics with 'statusChangedAt' instead"  #-}

instance Core.FromJSON Operation where
        parseJSON
          = Core.withObject "Operation" Core.$
              \ x ->
                Operation' Core.<$>
                  (x Core..:? "createdAt") Core.<*> x Core..:? "errorCode" Core.<*>
                    x Core..:? "errorDetails"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "isTerminal"
                    Core.<*> x Core..:? "location"
                    Core.<*> x Core..:? "operationDetails"
                    Core.<*> x Core..:? "operationType"
                    Core.<*> x Core..:? "resourceName"
                    Core.<*> x Core..:? "resourceType"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "statusChangedAt"
