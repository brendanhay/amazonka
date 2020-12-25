{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.Operation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.Operation
  ( Operation (..),

    -- * Smart constructor
    mkOperation,

    -- * Lenses
    oCreateDate,
    oErrorCode,
    oErrorMessage,
    oId,
    oStatus,
    oTargets,
    oType,
    oUpdateDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53AutoNaming.Types.ErrorCode as Types
import qualified Network.AWS.Route53AutoNaming.Types.ErrorMessage as Types
import qualified Network.AWS.Route53AutoNaming.Types.Id as Types
import qualified Network.AWS.Route53AutoNaming.Types.OperationStatus as Types
import qualified Network.AWS.Route53AutoNaming.Types.OperationTargetType as Types
import qualified Network.AWS.Route53AutoNaming.Types.OperationType as Types
import qualified Network.AWS.Route53AutoNaming.Types.ResourceId as Types

-- | A complex type that contains information about a specified operation.
--
-- /See:/ 'mkOperation' smart constructor.
data Operation = Operation'
  { -- | The date and time that the request was submitted, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
    createDate :: Core.Maybe Core.NominalDiffTime,
    -- | The code associated with @ErrorMessage@ . Values for @ErrorCode@ include the following:
    --
    --
    --     * @ACCESS_DENIED@
    --
    --
    --     * @CANNOT_CREATE_HOSTED_ZONE@
    --
    --
    --     * @EXPIRED_TOKEN@
    --
    --
    --     * @HOSTED_ZONE_NOT_FOUND@
    --
    --
    --     * @INTERNAL_FAILURE@
    --
    --
    --     * @INVALID_CHANGE_BATCH@
    --
    --
    --     * @THROTTLED_REQUEST@
    errorCode :: Core.Maybe Types.ErrorCode,
    -- | If the value of @Status@ is @FAIL@ , the reason that the operation failed.
    errorMessage :: Core.Maybe Types.ErrorMessage,
    -- | The ID of the operation that you want to get information about.
    id :: Core.Maybe Types.Id,
    -- | The status of the operation. Values include the following:
    --
    --
    --     * __SUBMITTED__ : This is the initial state immediately after you submit a request.
    --
    --
    --     * __PENDING__ : AWS Cloud Map is performing the operation.
    --
    --
    --     * __SUCCESS__ : The operation succeeded.
    --
    --
    --     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
    status :: Core.Maybe Types.OperationStatus,
    -- | The name of the target entity that is associated with the operation:
    --
    --
    --     * __NAMESPACE__ : The namespace ID is returned in the @ResourceId@ property.
    --
    --
    --     * __SERVICE__ : The service ID is returned in the @ResourceId@ property.
    --
    --
    --     * __INSTANCE__ : The instance ID is returned in the @ResourceId@ property.
    targets :: Core.Maybe (Core.HashMap Types.OperationTargetType Types.ResourceId),
    -- | The name of the operation that is associated with the specified ID.
    type' :: Core.Maybe Types.OperationType,
    -- | The date and time that the value of @Status@ changed to the current value, in Unix date/time format and Coordinated Universal Time (UTC). The value of @UpdateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
    updateDate :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Operation' value with any optional fields omitted.
mkOperation ::
  Operation
mkOperation =
  Operation'
    { createDate = Core.Nothing,
      errorCode = Core.Nothing,
      errorMessage = Core.Nothing,
      id = Core.Nothing,
      status = Core.Nothing,
      targets = Core.Nothing,
      type' = Core.Nothing,
      updateDate = Core.Nothing
    }

-- | The date and time that the request was submitted, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCreateDate :: Lens.Lens' Operation (Core.Maybe Core.NominalDiffTime)
oCreateDate = Lens.field @"createDate"
{-# DEPRECATED oCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The code associated with @ErrorMessage@ . Values for @ErrorCode@ include the following:
--
--
--     * @ACCESS_DENIED@
--
--
--     * @CANNOT_CREATE_HOSTED_ZONE@
--
--
--     * @EXPIRED_TOKEN@
--
--
--     * @HOSTED_ZONE_NOT_FOUND@
--
--
--     * @INTERNAL_FAILURE@
--
--
--     * @INVALID_CHANGE_BATCH@
--
--
--     * @THROTTLED_REQUEST@
--
--
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oErrorCode :: Lens.Lens' Operation (Core.Maybe Types.ErrorCode)
oErrorCode = Lens.field @"errorCode"
{-# DEPRECATED oErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | If the value of @Status@ is @FAIL@ , the reason that the operation failed.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oErrorMessage :: Lens.Lens' Operation (Core.Maybe Types.ErrorMessage)
oErrorMessage = Lens.field @"errorMessage"
{-# DEPRECATED oErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The ID of the operation that you want to get information about.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Operation (Core.Maybe Types.Id)
oId = Lens.field @"id"
{-# DEPRECATED oId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The status of the operation. Values include the following:
--
--
--     * __SUBMITTED__ : This is the initial state immediately after you submit a request.
--
--
--     * __PENDING__ : AWS Cloud Map is performing the operation.
--
--
--     * __SUCCESS__ : The operation succeeded.
--
--
--     * __FAIL__ : The operation failed. For the failure reason, see @ErrorMessage@ .
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oStatus :: Lens.Lens' Operation (Core.Maybe Types.OperationStatus)
oStatus = Lens.field @"status"
{-# DEPRECATED oStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the target entity that is associated with the operation:
--
--
--     * __NAMESPACE__ : The namespace ID is returned in the @ResourceId@ property.
--
--
--     * __SERVICE__ : The service ID is returned in the @ResourceId@ property.
--
--
--     * __INSTANCE__ : The instance ID is returned in the @ResourceId@ property.
--
--
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oTargets :: Lens.Lens' Operation (Core.Maybe (Core.HashMap Types.OperationTargetType Types.ResourceId))
oTargets = Lens.field @"targets"
{-# DEPRECATED oTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | The name of the operation that is associated with the specified ID.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oType :: Lens.Lens' Operation (Core.Maybe Types.OperationType)
oType = Lens.field @"type'"
{-# DEPRECATED oType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The date and time that the value of @Status@ changed to the current value, in Unix date/time format and Coordinated Universal Time (UTC). The value of @UpdateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oUpdateDate :: Lens.Lens' Operation (Core.Maybe Core.NominalDiffTime)
oUpdateDate = Lens.field @"updateDate"
{-# DEPRECATED oUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

instance Core.FromJSON Operation where
  parseJSON =
    Core.withObject "Operation" Core.$
      \x ->
        Operation'
          Core.<$> (x Core..:? "CreateDate")
          Core.<*> (x Core..:? "ErrorCode")
          Core.<*> (x Core..:? "ErrorMessage")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "Targets")
          Core.<*> (x Core..:? "Type")
          Core.<*> (x Core..:? "UpdateDate")
