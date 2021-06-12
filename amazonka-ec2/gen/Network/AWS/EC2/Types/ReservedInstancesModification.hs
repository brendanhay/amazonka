{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstancesModification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstancesModification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ReservedInstancesId
import Network.AWS.EC2.Types.ReservedInstancesModificationResult
import qualified Network.AWS.Lens as Lens

-- | Describes a Reserved Instance modification.
--
-- /See:/ 'newReservedInstancesModification' smart constructor.
data ReservedInstancesModification = ReservedInstancesModification'
  { -- | The reason for the status.
    statusMessage :: Core.Maybe Core.Text,
    -- | The status of the Reserved Instances modification request.
    status :: Core.Maybe Core.Text,
    -- | The time when the modification request was created.
    createDate :: Core.Maybe Core.ISO8601,
    -- | Contains target configurations along with their corresponding new
    -- Reserved Instance IDs.
    modificationResults :: Core.Maybe [ReservedInstancesModificationResult],
    -- | The time for the modification to become effective.
    effectiveDate :: Core.Maybe Core.ISO8601,
    -- | The IDs of one or more Reserved Instances.
    reservedInstancesIds :: Core.Maybe [ReservedInstancesId],
    -- | A unique ID for the Reserved Instance modification.
    reservedInstancesModificationId :: Core.Maybe Core.Text,
    -- | The time when the modification request was last updated.
    updateDate :: Core.Maybe Core.ISO8601,
    -- | A unique, case-sensitive key supplied by the client to ensure that the
    -- request is idempotent. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservedInstancesModification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'reservedInstancesModification_statusMessage' - The reason for the status.
--
-- 'status', 'reservedInstancesModification_status' - The status of the Reserved Instances modification request.
--
-- 'createDate', 'reservedInstancesModification_createDate' - The time when the modification request was created.
--
-- 'modificationResults', 'reservedInstancesModification_modificationResults' - Contains target configurations along with their corresponding new
-- Reserved Instance IDs.
--
-- 'effectiveDate', 'reservedInstancesModification_effectiveDate' - The time for the modification to become effective.
--
-- 'reservedInstancesIds', 'reservedInstancesModification_reservedInstancesIds' - The IDs of one or more Reserved Instances.
--
-- 'reservedInstancesModificationId', 'reservedInstancesModification_reservedInstancesModificationId' - A unique ID for the Reserved Instance modification.
--
-- 'updateDate', 'reservedInstancesModification_updateDate' - The time when the modification request was last updated.
--
-- 'clientToken', 'reservedInstancesModification_clientToken' - A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
newReservedInstancesModification ::
  ReservedInstancesModification
newReservedInstancesModification =
  ReservedInstancesModification'
    { statusMessage =
        Core.Nothing,
      status = Core.Nothing,
      createDate = Core.Nothing,
      modificationResults = Core.Nothing,
      effectiveDate = Core.Nothing,
      reservedInstancesIds = Core.Nothing,
      reservedInstancesModificationId =
        Core.Nothing,
      updateDate = Core.Nothing,
      clientToken = Core.Nothing
    }

-- | The reason for the status.
reservedInstancesModification_statusMessage :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.Text)
reservedInstancesModification_statusMessage = Lens.lens (\ReservedInstancesModification' {statusMessage} -> statusMessage) (\s@ReservedInstancesModification' {} a -> s {statusMessage = a} :: ReservedInstancesModification)

-- | The status of the Reserved Instances modification request.
reservedInstancesModification_status :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.Text)
reservedInstancesModification_status = Lens.lens (\ReservedInstancesModification' {status} -> status) (\s@ReservedInstancesModification' {} a -> s {status = a} :: ReservedInstancesModification)

-- | The time when the modification request was created.
reservedInstancesModification_createDate :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.UTCTime)
reservedInstancesModification_createDate = Lens.lens (\ReservedInstancesModification' {createDate} -> createDate) (\s@ReservedInstancesModification' {} a -> s {createDate = a} :: ReservedInstancesModification) Core.. Lens.mapping Core._Time

-- | Contains target configurations along with their corresponding new
-- Reserved Instance IDs.
reservedInstancesModification_modificationResults :: Lens.Lens' ReservedInstancesModification (Core.Maybe [ReservedInstancesModificationResult])
reservedInstancesModification_modificationResults = Lens.lens (\ReservedInstancesModification' {modificationResults} -> modificationResults) (\s@ReservedInstancesModification' {} a -> s {modificationResults = a} :: ReservedInstancesModification) Core.. Lens.mapping Lens._Coerce

-- | The time for the modification to become effective.
reservedInstancesModification_effectiveDate :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.UTCTime)
reservedInstancesModification_effectiveDate = Lens.lens (\ReservedInstancesModification' {effectiveDate} -> effectiveDate) (\s@ReservedInstancesModification' {} a -> s {effectiveDate = a} :: ReservedInstancesModification) Core.. Lens.mapping Core._Time

-- | The IDs of one or more Reserved Instances.
reservedInstancesModification_reservedInstancesIds :: Lens.Lens' ReservedInstancesModification (Core.Maybe [ReservedInstancesId])
reservedInstancesModification_reservedInstancesIds = Lens.lens (\ReservedInstancesModification' {reservedInstancesIds} -> reservedInstancesIds) (\s@ReservedInstancesModification' {} a -> s {reservedInstancesIds = a} :: ReservedInstancesModification) Core.. Lens.mapping Lens._Coerce

-- | A unique ID for the Reserved Instance modification.
reservedInstancesModification_reservedInstancesModificationId :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.Text)
reservedInstancesModification_reservedInstancesModificationId = Lens.lens (\ReservedInstancesModification' {reservedInstancesModificationId} -> reservedInstancesModificationId) (\s@ReservedInstancesModification' {} a -> s {reservedInstancesModificationId = a} :: ReservedInstancesModification)

-- | The time when the modification request was last updated.
reservedInstancesModification_updateDate :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.UTCTime)
reservedInstancesModification_updateDate = Lens.lens (\ReservedInstancesModification' {updateDate} -> updateDate) (\s@ReservedInstancesModification' {} a -> s {updateDate = a} :: ReservedInstancesModification) Core.. Lens.mapping Core._Time

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
reservedInstancesModification_clientToken :: Lens.Lens' ReservedInstancesModification (Core.Maybe Core.Text)
reservedInstancesModification_clientToken = Lens.lens (\ReservedInstancesModification' {clientToken} -> clientToken) (\s@ReservedInstancesModification' {} a -> s {clientToken = a} :: ReservedInstancesModification)

instance Core.FromXML ReservedInstancesModification where
  parseXML x =
    ReservedInstancesModification'
      Core.<$> (x Core..@? "statusMessage")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "createDate")
      Core.<*> ( x Core..@? "modificationResultSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "effectiveDate")
      Core.<*> ( x Core..@? "reservedInstancesSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "reservedInstancesModificationId")
      Core.<*> (x Core..@? "updateDate")
      Core.<*> (x Core..@? "clientToken")

instance Core.Hashable ReservedInstancesModification

instance Core.NFData ReservedInstancesModification
