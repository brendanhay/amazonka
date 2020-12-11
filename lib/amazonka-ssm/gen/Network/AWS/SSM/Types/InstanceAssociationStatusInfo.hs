-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.InstanceAssociationStatusInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.InstanceAssociationStatusInfo
  ( InstanceAssociationStatusInfo (..),

    -- * Smart constructor
    mkInstanceAssociationStatusInfo,

    -- * Lenses
    iasiAssociationId,
    iasiInstanceId,
    iasiDetailedStatus,
    iasiStatus,
    iasiOutputURL,
    iasiExecutionSummary,
    iasiName,
    iasiErrorCode,
    iasiDocumentVersion,
    iasiAssociationVersion,
    iasiExecutionDate,
    iasiAssociationName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.InstanceAssociationOutputURL

-- | Status information about the instance association.
--
-- /See:/ 'mkInstanceAssociationStatusInfo' smart constructor.
data InstanceAssociationStatusInfo = InstanceAssociationStatusInfo'
  { associationId ::
      Lude.Maybe Lude.Text,
    instanceId ::
      Lude.Maybe Lude.Text,
    detailedStatus ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe Lude.Text,
    outputURL ::
      Lude.Maybe
        InstanceAssociationOutputURL,
    executionSummary ::
      Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    errorCode ::
      Lude.Maybe Lude.Text,
    documentVersion ::
      Lude.Maybe Lude.Text,
    associationVersion ::
      Lude.Maybe Lude.Text,
    executionDate ::
      Lude.Maybe Lude.Timestamp,
    associationName ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceAssociationStatusInfo' with the minimum fields required to make a request.
--
-- * 'associationId' - The association ID.
-- * 'associationName' - The name of the association applied to the instance.
-- * 'associationVersion' - The version of the association applied to the instance.
-- * 'detailedStatus' - Detailed status information about the instance association.
-- * 'documentVersion' - The association document versions.
-- * 'errorCode' - An error code returned by the request to create the association.
-- * 'executionDate' - The date the instance association ran.
-- * 'executionSummary' - Summary information about association execution.
-- * 'instanceId' - The instance ID where the association was created.
-- * 'name' - The name of the association.
-- * 'outputURL' - A URL for an S3 bucket where you want to store the results of this request.
-- * 'status' - Status information about the instance association.
mkInstanceAssociationStatusInfo ::
  InstanceAssociationStatusInfo
mkInstanceAssociationStatusInfo =
  InstanceAssociationStatusInfo'
    { associationId = Lude.Nothing,
      instanceId = Lude.Nothing,
      detailedStatus = Lude.Nothing,
      status = Lude.Nothing,
      outputURL = Lude.Nothing,
      executionSummary = Lude.Nothing,
      name = Lude.Nothing,
      errorCode = Lude.Nothing,
      documentVersion = Lude.Nothing,
      associationVersion = Lude.Nothing,
      executionDate = Lude.Nothing,
      associationName = Lude.Nothing
    }

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiAssociationId :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Text)
iasiAssociationId = Lens.lens (associationId :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The instance ID where the association was created.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiInstanceId :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Text)
iasiInstanceId = Lens.lens (instanceId :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | Detailed status information about the instance association.
--
-- /Note:/ Consider using 'detailedStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiDetailedStatus :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Text)
iasiDetailedStatus = Lens.lens (detailedStatus :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {detailedStatus = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiDetailedStatus "Use generic-lens or generic-optics with 'detailedStatus' instead." #-}

-- | Status information about the instance association.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiStatus :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Text)
iasiStatus = Lens.lens (status :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A URL for an S3 bucket where you want to store the results of this request.
--
-- /Note:/ Consider using 'outputURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiOutputURL :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe InstanceAssociationOutputURL)
iasiOutputURL = Lens.lens (outputURL :: InstanceAssociationStatusInfo -> Lude.Maybe InstanceAssociationOutputURL) (\s a -> s {outputURL = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiOutputURL "Use generic-lens or generic-optics with 'outputURL' instead." #-}

-- | Summary information about association execution.
--
-- /Note:/ Consider using 'executionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiExecutionSummary :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Text)
iasiExecutionSummary = Lens.lens (executionSummary :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {executionSummary = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiExecutionSummary "Use generic-lens or generic-optics with 'executionSummary' instead." #-}

-- | The name of the association.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiName :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Text)
iasiName = Lens.lens (name :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An error code returned by the request to create the association.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiErrorCode :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Text)
iasiErrorCode = Lens.lens (errorCode :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The association document versions.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiDocumentVersion :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Text)
iasiDocumentVersion = Lens.lens (documentVersion :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The version of the association applied to the instance.
--
-- /Note:/ Consider using 'associationVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiAssociationVersion :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Text)
iasiAssociationVersion = Lens.lens (associationVersion :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {associationVersion = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiAssociationVersion "Use generic-lens or generic-optics with 'associationVersion' instead." #-}

-- | The date the instance association ran.
--
-- /Note:/ Consider using 'executionDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiExecutionDate :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Timestamp)
iasiExecutionDate = Lens.lens (executionDate :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {executionDate = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiExecutionDate "Use generic-lens or generic-optics with 'executionDate' instead." #-}

-- | The name of the association applied to the instance.
--
-- /Note:/ Consider using 'associationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iasiAssociationName :: Lens.Lens' InstanceAssociationStatusInfo (Lude.Maybe Lude.Text)
iasiAssociationName = Lens.lens (associationName :: InstanceAssociationStatusInfo -> Lude.Maybe Lude.Text) (\s a -> s {associationName = a} :: InstanceAssociationStatusInfo)
{-# DEPRECATED iasiAssociationName "Use generic-lens or generic-optics with 'associationName' instead." #-}

instance Lude.FromJSON InstanceAssociationStatusInfo where
  parseJSON =
    Lude.withObject
      "InstanceAssociationStatusInfo"
      ( \x ->
          InstanceAssociationStatusInfo'
            Lude.<$> (x Lude..:? "AssociationId")
            Lude.<*> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "DetailedStatus")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "OutputUrl")
            Lude.<*> (x Lude..:? "ExecutionSummary")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "DocumentVersion")
            Lude.<*> (x Lude..:? "AssociationVersion")
            Lude.<*> (x Lude..:? "ExecutionDate")
            Lude.<*> (x Lude..:? "AssociationName")
      )
