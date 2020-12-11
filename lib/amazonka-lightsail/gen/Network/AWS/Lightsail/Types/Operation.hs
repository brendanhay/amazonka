-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Operation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Operation
  ( Operation (..),

    -- * Smart constructor
    mkOperation,

    -- * Lenses
    opeStatus,
    opeOperationDetails,
    opeResourceType,
    opeCreatedAt,
    opeResourceName,
    opeLocation,
    opeStatusChangedAt,
    opeErrorDetails,
    opeErrorCode,
    opeId,
    opeOperationType,
    opeIsTerminal,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.OperationStatus
import Network.AWS.Lightsail.Types.OperationType
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import qualified Network.AWS.Prelude as Lude

-- | Describes the API operation.
--
-- /See:/ 'mkOperation' smart constructor.
data Operation = Operation'
  { status :: Lude.Maybe OperationStatus,
    operationDetails :: Lude.Maybe Lude.Text,
    resourceType :: Lude.Maybe ResourceType,
    createdAt :: Lude.Maybe Lude.Timestamp,
    resourceName :: Lude.Maybe Lude.Text,
    location :: Lude.Maybe ResourceLocation,
    statusChangedAt :: Lude.Maybe Lude.Timestamp,
    errorDetails :: Lude.Maybe Lude.Text,
    errorCode :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    operationType :: Lude.Maybe OperationType,
    isTerminal :: Lude.Maybe Lude.Bool
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
-- * 'createdAt' - The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
-- * 'errorCode' - The error code.
-- * 'errorDetails' - The error details.
-- * 'id' - The ID of the operation.
-- * 'isTerminal' - A Boolean value indicating whether the operation is terminal.
-- * 'location' - The AWS Region and Availability Zone.
-- * 'operationDetails' - Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
-- * 'operationType' - The type of operation.
-- * 'resourceName' - The resource name.
-- * 'resourceType' - The resource type.
-- * 'status' - The status of the operation.
-- * 'statusChangedAt' - The timestamp when the status was changed (e.g., @1479816991.349@ ).
mkOperation ::
  Operation
mkOperation =
  Operation'
    { status = Lude.Nothing,
      operationDetails = Lude.Nothing,
      resourceType = Lude.Nothing,
      createdAt = Lude.Nothing,
      resourceName = Lude.Nothing,
      location = Lude.Nothing,
      statusChangedAt = Lude.Nothing,
      errorDetails = Lude.Nothing,
      errorCode = Lude.Nothing,
      id = Lude.Nothing,
      operationType = Lude.Nothing,
      isTerminal = Lude.Nothing
    }

-- | The status of the operation.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeStatus :: Lens.Lens' Operation (Lude.Maybe OperationStatus)
opeStatus = Lens.lens (status :: Operation -> Lude.Maybe OperationStatus) (\s a -> s {status = a} :: Operation)
{-# DEPRECATED opeStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
--
-- /Note:/ Consider using 'operationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeOperationDetails :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
opeOperationDetails = Lens.lens (operationDetails :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {operationDetails = a} :: Operation)
{-# DEPRECATED opeOperationDetails "Use generic-lens or generic-optics with 'operationDetails' instead." #-}

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeResourceType :: Lens.Lens' Operation (Lude.Maybe ResourceType)
opeResourceType = Lens.lens (resourceType :: Operation -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: Operation)
{-# DEPRECATED opeResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeCreatedAt :: Lens.Lens' Operation (Lude.Maybe Lude.Timestamp)
opeCreatedAt = Lens.lens (createdAt :: Operation -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Operation)
{-# DEPRECATED opeCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The resource name.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeResourceName :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
opeResourceName = Lens.lens (resourceName :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: Operation)
{-# DEPRECATED opeResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The AWS Region and Availability Zone.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeLocation :: Lens.Lens' Operation (Lude.Maybe ResourceLocation)
opeLocation = Lens.lens (location :: Operation -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: Operation)
{-# DEPRECATED opeLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The timestamp when the status was changed (e.g., @1479816991.349@ ).
--
-- /Note:/ Consider using 'statusChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeStatusChangedAt :: Lens.Lens' Operation (Lude.Maybe Lude.Timestamp)
opeStatusChangedAt = Lens.lens (statusChangedAt :: Operation -> Lude.Maybe Lude.Timestamp) (\s a -> s {statusChangedAt = a} :: Operation)
{-# DEPRECATED opeStatusChangedAt "Use generic-lens or generic-optics with 'statusChangedAt' instead." #-}

-- | The error details.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeErrorDetails :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
opeErrorDetails = Lens.lens (errorDetails :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {errorDetails = a} :: Operation)
{-# DEPRECATED opeErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeErrorCode :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
opeErrorCode = Lens.lens (errorCode :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: Operation)
{-# DEPRECATED opeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The ID of the operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeId :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
opeId = Lens.lens (id :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Operation)
{-# DEPRECATED opeId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of operation.
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeOperationType :: Lens.Lens' Operation (Lude.Maybe OperationType)
opeOperationType = Lens.lens (operationType :: Operation -> Lude.Maybe OperationType) (\s a -> s {operationType = a} :: Operation)
{-# DEPRECATED opeOperationType "Use generic-lens or generic-optics with 'operationType' instead." #-}

-- | A Boolean value indicating whether the operation is terminal.
--
-- /Note:/ Consider using 'isTerminal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
opeIsTerminal :: Lens.Lens' Operation (Lude.Maybe Lude.Bool)
opeIsTerminal = Lens.lens (isTerminal :: Operation -> Lude.Maybe Lude.Bool) (\s a -> s {isTerminal = a} :: Operation)
{-# DEPRECATED opeIsTerminal "Use generic-lens or generic-optics with 'isTerminal' instead." #-}

instance Lude.FromJSON Operation where
  parseJSON =
    Lude.withObject
      "Operation"
      ( \x ->
          Operation'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "operationDetails")
            Lude.<*> (x Lude..:? "resourceType")
            Lude.<*> (x Lude..:? "createdAt")
            Lude.<*> (x Lude..:? "resourceName")
            Lude.<*> (x Lude..:? "location")
            Lude.<*> (x Lude..:? "statusChangedAt")
            Lude.<*> (x Lude..:? "errorDetails")
            Lude.<*> (x Lude..:? "errorCode")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "operationType")
            Lude.<*> (x Lude..:? "isTerminal")
      )
