-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesModification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesModification
  ( ReservedInstancesModification (..),

    -- * Smart constructor
    mkReservedInstancesModification,

    -- * Lenses
    rimModificationResults,
    rimStatus,
    rimClientToken,
    rimUpdateDate,
    rimCreateDate,
    rimEffectiveDate,
    rimStatusMessage,
    rimReservedInstancesModificationId,
    rimReservedInstancesIds,
  )
where

import Network.AWS.EC2.Types.ReservedInstancesId
import Network.AWS.EC2.Types.ReservedInstancesModificationResult
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Reserved Instance modification.
--
-- /See:/ 'mkReservedInstancesModification' smart constructor.
data ReservedInstancesModification = ReservedInstancesModification'
  { modificationResults ::
      Lude.Maybe
        [ReservedInstancesModificationResult],
    status :: Lude.Maybe Lude.Text,
    clientToken ::
      Lude.Maybe Lude.Text,
    updateDate ::
      Lude.Maybe Lude.ISO8601,
    createDate ::
      Lude.Maybe Lude.ISO8601,
    effectiveDate ::
      Lude.Maybe Lude.ISO8601,
    statusMessage ::
      Lude.Maybe Lude.Text,
    reservedInstancesModificationId ::
      Lude.Maybe Lude.Text,
    reservedInstancesIds ::
      Lude.Maybe
        [ReservedInstancesId]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedInstancesModification' with the minimum fields required to make a request.
--
-- * 'clientToken' - A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
-- * 'createDate' - The time when the modification request was created.
-- * 'effectiveDate' - The time for the modification to become effective.
-- * 'modificationResults' - Contains target configurations along with their corresponding new Reserved Instance IDs.
-- * 'reservedInstancesIds' - The IDs of one or more Reserved Instances.
-- * 'reservedInstancesModificationId' - A unique ID for the Reserved Instance modification.
-- * 'status' - The status of the Reserved Instances modification request.
-- * 'statusMessage' - The reason for the status.
-- * 'updateDate' - The time when the modification request was last updated.
mkReservedInstancesModification ::
  ReservedInstancesModification
mkReservedInstancesModification =
  ReservedInstancesModification'
    { modificationResults =
        Lude.Nothing,
      status = Lude.Nothing,
      clientToken = Lude.Nothing,
      updateDate = Lude.Nothing,
      createDate = Lude.Nothing,
      effectiveDate = Lude.Nothing,
      statusMessage = Lude.Nothing,
      reservedInstancesModificationId = Lude.Nothing,
      reservedInstancesIds = Lude.Nothing
    }

-- | Contains target configurations along with their corresponding new Reserved Instance IDs.
--
-- /Note:/ Consider using 'modificationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimModificationResults :: Lens.Lens' ReservedInstancesModification (Lude.Maybe [ReservedInstancesModificationResult])
rimModificationResults = Lens.lens (modificationResults :: ReservedInstancesModification -> Lude.Maybe [ReservedInstancesModificationResult]) (\s a -> s {modificationResults = a} :: ReservedInstancesModification)
{-# DEPRECATED rimModificationResults "Use generic-lens or generic-optics with 'modificationResults' instead." #-}

-- | The status of the Reserved Instances modification request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimStatus :: Lens.Lens' ReservedInstancesModification (Lude.Maybe Lude.Text)
rimStatus = Lens.lens (status :: ReservedInstancesModification -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ReservedInstancesModification)
{-# DEPRECATED rimStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A unique, case-sensitive key supplied by the client to ensure that the request is idempotent. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency> .
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimClientToken :: Lens.Lens' ReservedInstancesModification (Lude.Maybe Lude.Text)
rimClientToken = Lens.lens (clientToken :: ReservedInstancesModification -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: ReservedInstancesModification)
{-# DEPRECATED rimClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The time when the modification request was last updated.
--
-- /Note:/ Consider using 'updateDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimUpdateDate :: Lens.Lens' ReservedInstancesModification (Lude.Maybe Lude.ISO8601)
rimUpdateDate = Lens.lens (updateDate :: ReservedInstancesModification -> Lude.Maybe Lude.ISO8601) (\s a -> s {updateDate = a} :: ReservedInstancesModification)
{-# DEPRECATED rimUpdateDate "Use generic-lens or generic-optics with 'updateDate' instead." #-}

-- | The time when the modification request was created.
--
-- /Note:/ Consider using 'createDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimCreateDate :: Lens.Lens' ReservedInstancesModification (Lude.Maybe Lude.ISO8601)
rimCreateDate = Lens.lens (createDate :: ReservedInstancesModification -> Lude.Maybe Lude.ISO8601) (\s a -> s {createDate = a} :: ReservedInstancesModification)
{-# DEPRECATED rimCreateDate "Use generic-lens or generic-optics with 'createDate' instead." #-}

-- | The time for the modification to become effective.
--
-- /Note:/ Consider using 'effectiveDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimEffectiveDate :: Lens.Lens' ReservedInstancesModification (Lude.Maybe Lude.ISO8601)
rimEffectiveDate = Lens.lens (effectiveDate :: ReservedInstancesModification -> Lude.Maybe Lude.ISO8601) (\s a -> s {effectiveDate = a} :: ReservedInstancesModification)
{-# DEPRECATED rimEffectiveDate "Use generic-lens or generic-optics with 'effectiveDate' instead." #-}

-- | The reason for the status.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimStatusMessage :: Lens.Lens' ReservedInstancesModification (Lude.Maybe Lude.Text)
rimStatusMessage = Lens.lens (statusMessage :: ReservedInstancesModification -> Lude.Maybe Lude.Text) (\s a -> s {statusMessage = a} :: ReservedInstancesModification)
{-# DEPRECATED rimStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | A unique ID for the Reserved Instance modification.
--
-- /Note:/ Consider using 'reservedInstancesModificationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimReservedInstancesModificationId :: Lens.Lens' ReservedInstancesModification (Lude.Maybe Lude.Text)
rimReservedInstancesModificationId = Lens.lens (reservedInstancesModificationId :: ReservedInstancesModification -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstancesModificationId = a} :: ReservedInstancesModification)
{-# DEPRECATED rimReservedInstancesModificationId "Use generic-lens or generic-optics with 'reservedInstancesModificationId' instead." #-}

-- | The IDs of one or more Reserved Instances.
--
-- /Note:/ Consider using 'reservedInstancesIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rimReservedInstancesIds :: Lens.Lens' ReservedInstancesModification (Lude.Maybe [ReservedInstancesId])
rimReservedInstancesIds = Lens.lens (reservedInstancesIds :: ReservedInstancesModification -> Lude.Maybe [ReservedInstancesId]) (\s a -> s {reservedInstancesIds = a} :: ReservedInstancesModification)
{-# DEPRECATED rimReservedInstancesIds "Use generic-lens or generic-optics with 'reservedInstancesIds' instead." #-}

instance Lude.FromXML ReservedInstancesModification where
  parseXML x =
    ReservedInstancesModification'
      Lude.<$> ( x Lude..@? "modificationResultSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "status")
      Lude.<*> (x Lude..@? "clientToken")
      Lude.<*> (x Lude..@? "updateDate")
      Lude.<*> (x Lude..@? "createDate")
      Lude.<*> (x Lude..@? "effectiveDate")
      Lude.<*> (x Lude..@? "statusMessage")
      Lude.<*> (x Lude..@? "reservedInstancesModificationId")
      Lude.<*> ( x Lude..@? "reservedInstancesSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
