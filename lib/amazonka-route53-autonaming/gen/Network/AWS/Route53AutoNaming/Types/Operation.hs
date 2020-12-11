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
    oStatus,
    oUpdateDate,
    oCreateDate,
    oTargets,
    oErrorCode,
    oId,
    oType,
    oErrorMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.OperationStatus
import Network.AWS.Route53AutoNaming.Types.OperationTargetType
import Network.AWS.Route53AutoNaming.Types.OperationType

-- | A complex type that contains information about a specified operation.
--
-- /See:/ 'mkOperation' smart constructor.
data Operation = Operation'
  { status :: Lude.Maybe OperationStatus,
    updateDate :: Lude.Maybe Lude.Timestamp,
    createDate :: Lude.Maybe Lude.Timestamp,
    targets ::
      Lude.Maybe (Lude.HashMap OperationTargetType (Lude.Text)),
    errorCode :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe OperationType,
    errorMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- * 'createDate' - The date and time that the request was submitted, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
-- * 'errorCode' - The code associated with @ErrorMessage@ . Values for @ErrorCode@ include the following:
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
-- * 'errorMessage' - If the value of @Status@ is @FAIL@ , the reason that the operation failed.
-- * 'id' - The ID of the operation that you want to get information about.
-- * 'status' - The status of the operation. Values include the following:
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
-- * 'targets' - The name of the target entity that is associated with the operation:
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
-- * 'type'' - The name of the operation that is associated with the specified ID.
-- * 'updateDate' - The date and time that the value of @Status@ changed to the current value, in Unix date/time format and Coordinated Universal Time (UTC). The value of @UpdateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
mkOperation ::
  Operation
mkOperation =
  Operation'
    { status = Lude.Nothing,
      updateDate = Lude.Nothing,
      createDate = Lude.Nothing,
      targets = Lude.Nothing,
      errorCode = Lude.Nothing,
      id = Lude.Nothing,
      type' = Lude.Nothing,
      errorMessage = Lude.Nothing
    }

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
oStatus :: Lens.Lens' Operation (Lude.Maybe OperationStatus)
oStatus = Lens.lens (status :: Operation -> Lude.Maybe OperationStatus) (\s a -> s {status = a} :: Operation)
{-# DEPRECATED oStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time that the value of @Status@ changed to the current value, in Unix date/time format and Coordinated Universal Time (UTC). The value of @UpdateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oUpdateDate :: Lens.Lens' Operation (Lude.Maybe Lude.Timestamp)
oUpdateDate = Lens.lens (updateDate :: Operation -> Lude.Maybe Lude.Timestamp) (\s a -> s {updateDate = a} :: Operation)
{-# DEPRECATED oUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

-- | The date and time that the request was submitted, in Unix date/time format and Coordinated Universal Time (UTC). The value of @CreateDate@ is accurate to milliseconds. For example, the value @1516925490.087@ represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCreateDate :: Lens.Lens' Operation (Lude.Maybe Lude.Timestamp)
oCreateDate = Lens.lens (createDate :: Operation -> Lude.Maybe Lude.Timestamp) (\s a -> s {createDate = a} :: Operation)
{-# DEPRECATED oCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

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
oTargets :: Lens.Lens' Operation (Lude.Maybe (Lude.HashMap OperationTargetType (Lude.Text)))
oTargets = Lens.lens (targets :: Operation -> Lude.Maybe (Lude.HashMap OperationTargetType (Lude.Text))) (\s a -> s {targets = a} :: Operation)
{-# DEPRECATED oTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

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
oErrorCode :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
oErrorCode = Lens.lens (errorCode :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: Operation)
{-# DEPRECATED oErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The ID of the operation that you want to get information about.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
oId = Lens.lens (id :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Operation)
{-# DEPRECATED oId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the operation that is associated with the specified ID.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oType :: Lens.Lens' Operation (Lude.Maybe OperationType)
oType = Lens.lens (type' :: Operation -> Lude.Maybe OperationType) (\s a -> s {type' = a} :: Operation)
{-# DEPRECATED oType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | If the value of @Status@ is @FAIL@ , the reason that the operation failed.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oErrorMessage :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
oErrorMessage = Lens.lens (errorMessage :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: Operation)
{-# DEPRECATED oErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

instance Lude.FromJSON Operation where
  parseJSON =
    Lude.withObject
      "Operation"
      ( \x ->
          Operation'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "UpdateDate")
            Lude.<*> (x Lude..:? "CreateDate")
            Lude.<*> (x Lude..:? "Targets" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "ErrorMessage")
      )
