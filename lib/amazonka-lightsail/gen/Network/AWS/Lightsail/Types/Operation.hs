{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    oStatus,
    oOperationDetails,
    oResourceType,
    oCreatedAt,
    oResourceName,
    oLocation,
    oStatusChangedAt,
    oErrorDetails,
    oErrorCode,
    oId,
    oOperationType,
    oIsTerminal,
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
  { -- | The status of the operation.
    status :: Lude.Maybe OperationStatus,
    -- | Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
    operationDetails :: Lude.Maybe Lude.Text,
    -- | The resource type.
    resourceType :: Lude.Maybe ResourceType,
    -- | The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
    createdAt :: Lude.Maybe Lude.Timestamp,
    -- | The resource name.
    resourceName :: Lude.Maybe Lude.Text,
    -- | The AWS Region and Availability Zone.
    location :: Lude.Maybe ResourceLocation,
    -- | The timestamp when the status was changed (e.g., @1479816991.349@ ).
    statusChangedAt :: Lude.Maybe Lude.Timestamp,
    -- | The error details.
    errorDetails :: Lude.Maybe Lude.Text,
    -- | The error code.
    errorCode :: Lude.Maybe Lude.Text,
    -- | The ID of the operation.
    id :: Lude.Maybe Lude.Text,
    -- | The type of operation.
    operationType :: Lude.Maybe OperationType,
    -- | A Boolean value indicating whether the operation is terminal.
    isTerminal :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Operation' with the minimum fields required to make a request.
--
-- * 'status' - The status of the operation.
-- * 'operationDetails' - Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
-- * 'resourceType' - The resource type.
-- * 'createdAt' - The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
-- * 'resourceName' - The resource name.
-- * 'location' - The AWS Region and Availability Zone.
-- * 'statusChangedAt' - The timestamp when the status was changed (e.g., @1479816991.349@ ).
-- * 'errorDetails' - The error details.
-- * 'errorCode' - The error code.
-- * 'id' - The ID of the operation.
-- * 'operationType' - The type of operation.
-- * 'isTerminal' - A Boolean value indicating whether the operation is terminal.
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
oStatus :: Lens.Lens' Operation (Lude.Maybe OperationStatus)
oStatus = Lens.lens (status :: Operation -> Lude.Maybe OperationStatus) (\s a -> s {status = a} :: Operation)
{-# DEPRECATED oStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | Details about the operation (e.g., @Debian-1GB-Ohio-1@ ).
--
-- /Note:/ Consider using 'operationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOperationDetails :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
oOperationDetails = Lens.lens (operationDetails :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {operationDetails = a} :: Operation)
{-# DEPRECATED oOperationDetails "Use generic-lens or generic-optics with 'operationDetails' instead." #-}

-- | The resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oResourceType :: Lens.Lens' Operation (Lude.Maybe ResourceType)
oResourceType = Lens.lens (resourceType :: Operation -> Lude.Maybe ResourceType) (\s a -> s {resourceType = a} :: Operation)
{-# DEPRECATED oResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The timestamp when the operation was initialized (e.g., @1479816991.349@ ).
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oCreatedAt :: Lens.Lens' Operation (Lude.Maybe Lude.Timestamp)
oCreatedAt = Lens.lens (createdAt :: Operation -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdAt = a} :: Operation)
{-# DEPRECATED oCreatedAt "Use generic-lens or generic-optics with 'createdAt' instead." #-}

-- | The resource name.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oResourceName :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
oResourceName = Lens.lens (resourceName :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {resourceName = a} :: Operation)
{-# DEPRECATED oResourceName "Use generic-lens or generic-optics with 'resourceName' instead." #-}

-- | The AWS Region and Availability Zone.
--
-- /Note:/ Consider using 'location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oLocation :: Lens.Lens' Operation (Lude.Maybe ResourceLocation)
oLocation = Lens.lens (location :: Operation -> Lude.Maybe ResourceLocation) (\s a -> s {location = a} :: Operation)
{-# DEPRECATED oLocation "Use generic-lens or generic-optics with 'location' instead." #-}

-- | The timestamp when the status was changed (e.g., @1479816991.349@ ).
--
-- /Note:/ Consider using 'statusChangedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oStatusChangedAt :: Lens.Lens' Operation (Lude.Maybe Lude.Timestamp)
oStatusChangedAt = Lens.lens (statusChangedAt :: Operation -> Lude.Maybe Lude.Timestamp) (\s a -> s {statusChangedAt = a} :: Operation)
{-# DEPRECATED oStatusChangedAt "Use generic-lens or generic-optics with 'statusChangedAt' instead." #-}

-- | The error details.
--
-- /Note:/ Consider using 'errorDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oErrorDetails :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
oErrorDetails = Lens.lens (errorDetails :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {errorDetails = a} :: Operation)
{-# DEPRECATED oErrorDetails "Use generic-lens or generic-optics with 'errorDetails' instead." #-}

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oErrorCode :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
oErrorCode = Lens.lens (errorCode :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: Operation)
{-# DEPRECATED oErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The ID of the operation.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oId :: Lens.Lens' Operation (Lude.Maybe Lude.Text)
oId = Lens.lens (id :: Operation -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Operation)
{-# DEPRECATED oId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of operation.
--
-- /Note:/ Consider using 'operationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oOperationType :: Lens.Lens' Operation (Lude.Maybe OperationType)
oOperationType = Lens.lens (operationType :: Operation -> Lude.Maybe OperationType) (\s a -> s {operationType = a} :: Operation)
{-# DEPRECATED oOperationType "Use generic-lens or generic-optics with 'operationType' instead." #-}

-- | A Boolean value indicating whether the operation is terminal.
--
-- /Note:/ Consider using 'isTerminal' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oIsTerminal :: Lens.Lens' Operation (Lude.Maybe Lude.Bool)
oIsTerminal = Lens.lens (isTerminal :: Operation -> Lude.Maybe Lude.Bool) (\s a -> s {isTerminal = a} :: Operation)
{-# DEPRECATED oIsTerminal "Use generic-lens or generic-optics with 'isTerminal' instead." #-}

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
