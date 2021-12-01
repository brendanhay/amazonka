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
-- Module      : Amazonka.EC2.Types.ReservedInstancesModification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservedInstancesModification where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ReservedInstancesId
import Amazonka.EC2.Types.ReservedInstancesModificationResult
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a Reserved Instance modification.
--
-- /See:/ 'newReservedInstancesModification' smart constructor.
data ReservedInstancesModification = ReservedInstancesModification'
  { -- | Contains target configurations along with their corresponding new
    -- Reserved Instance IDs.
    modificationResults :: Prelude.Maybe [ReservedInstancesModificationResult],
    -- | The status of the Reserved Instances modification request.
    status :: Prelude.Maybe Prelude.Text,
    -- | A unique, case-sensitive key supplied by the client to ensure that the
    -- request is idempotent. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The time when the modification request was last updated.
    updateDate :: Prelude.Maybe Core.ISO8601,
    -- | The time when the modification request was created.
    createDate :: Prelude.Maybe Core.ISO8601,
    -- | The time for the modification to become effective.
    effectiveDate :: Prelude.Maybe Core.ISO8601,
    -- | The reason for the status.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | A unique ID for the Reserved Instance modification.
    reservedInstancesModificationId :: Prelude.Maybe Prelude.Text,
    -- | The IDs of one or more Reserved Instances.
    reservedInstancesIds :: Prelude.Maybe [ReservedInstancesId]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstancesModification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modificationResults', 'reservedInstancesModification_modificationResults' - Contains target configurations along with their corresponding new
-- Reserved Instance IDs.
--
-- 'status', 'reservedInstancesModification_status' - The status of the Reserved Instances modification request.
--
-- 'clientToken', 'reservedInstancesModification_clientToken' - A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'updateDate', 'reservedInstancesModification_updateDate' - The time when the modification request was last updated.
--
-- 'createDate', 'reservedInstancesModification_createDate' - The time when the modification request was created.
--
-- 'effectiveDate', 'reservedInstancesModification_effectiveDate' - The time for the modification to become effective.
--
-- 'statusMessage', 'reservedInstancesModification_statusMessage' - The reason for the status.
--
-- 'reservedInstancesModificationId', 'reservedInstancesModification_reservedInstancesModificationId' - A unique ID for the Reserved Instance modification.
--
-- 'reservedInstancesIds', 'reservedInstancesModification_reservedInstancesIds' - The IDs of one or more Reserved Instances.
newReservedInstancesModification ::
  ReservedInstancesModification
newReservedInstancesModification =
  ReservedInstancesModification'
    { modificationResults =
        Prelude.Nothing,
      status = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      updateDate = Prelude.Nothing,
      createDate = Prelude.Nothing,
      effectiveDate = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      reservedInstancesModificationId =
        Prelude.Nothing,
      reservedInstancesIds = Prelude.Nothing
    }

-- | Contains target configurations along with their corresponding new
-- Reserved Instance IDs.
reservedInstancesModification_modificationResults :: Lens.Lens' ReservedInstancesModification (Prelude.Maybe [ReservedInstancesModificationResult])
reservedInstancesModification_modificationResults = Lens.lens (\ReservedInstancesModification' {modificationResults} -> modificationResults) (\s@ReservedInstancesModification' {} a -> s {modificationResults = a} :: ReservedInstancesModification) Prelude.. Lens.mapping Lens.coerced

-- | The status of the Reserved Instances modification request.
reservedInstancesModification_status :: Lens.Lens' ReservedInstancesModification (Prelude.Maybe Prelude.Text)
reservedInstancesModification_status = Lens.lens (\ReservedInstancesModification' {status} -> status) (\s@ReservedInstancesModification' {} a -> s {status = a} :: ReservedInstancesModification)

-- | A unique, case-sensitive key supplied by the client to ensure that the
-- request is idempotent. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
reservedInstancesModification_clientToken :: Lens.Lens' ReservedInstancesModification (Prelude.Maybe Prelude.Text)
reservedInstancesModification_clientToken = Lens.lens (\ReservedInstancesModification' {clientToken} -> clientToken) (\s@ReservedInstancesModification' {} a -> s {clientToken = a} :: ReservedInstancesModification)

-- | The time when the modification request was last updated.
reservedInstancesModification_updateDate :: Lens.Lens' ReservedInstancesModification (Prelude.Maybe Prelude.UTCTime)
reservedInstancesModification_updateDate = Lens.lens (\ReservedInstancesModification' {updateDate} -> updateDate) (\s@ReservedInstancesModification' {} a -> s {updateDate = a} :: ReservedInstancesModification) Prelude.. Lens.mapping Core._Time

-- | The time when the modification request was created.
reservedInstancesModification_createDate :: Lens.Lens' ReservedInstancesModification (Prelude.Maybe Prelude.UTCTime)
reservedInstancesModification_createDate = Lens.lens (\ReservedInstancesModification' {createDate} -> createDate) (\s@ReservedInstancesModification' {} a -> s {createDate = a} :: ReservedInstancesModification) Prelude.. Lens.mapping Core._Time

-- | The time for the modification to become effective.
reservedInstancesModification_effectiveDate :: Lens.Lens' ReservedInstancesModification (Prelude.Maybe Prelude.UTCTime)
reservedInstancesModification_effectiveDate = Lens.lens (\ReservedInstancesModification' {effectiveDate} -> effectiveDate) (\s@ReservedInstancesModification' {} a -> s {effectiveDate = a} :: ReservedInstancesModification) Prelude.. Lens.mapping Core._Time

-- | The reason for the status.
reservedInstancesModification_statusMessage :: Lens.Lens' ReservedInstancesModification (Prelude.Maybe Prelude.Text)
reservedInstancesModification_statusMessage = Lens.lens (\ReservedInstancesModification' {statusMessage} -> statusMessage) (\s@ReservedInstancesModification' {} a -> s {statusMessage = a} :: ReservedInstancesModification)

-- | A unique ID for the Reserved Instance modification.
reservedInstancesModification_reservedInstancesModificationId :: Lens.Lens' ReservedInstancesModification (Prelude.Maybe Prelude.Text)
reservedInstancesModification_reservedInstancesModificationId = Lens.lens (\ReservedInstancesModification' {reservedInstancesModificationId} -> reservedInstancesModificationId) (\s@ReservedInstancesModification' {} a -> s {reservedInstancesModificationId = a} :: ReservedInstancesModification)

-- | The IDs of one or more Reserved Instances.
reservedInstancesModification_reservedInstancesIds :: Lens.Lens' ReservedInstancesModification (Prelude.Maybe [ReservedInstancesId])
reservedInstancesModification_reservedInstancesIds = Lens.lens (\ReservedInstancesModification' {reservedInstancesIds} -> reservedInstancesIds) (\s@ReservedInstancesModification' {} a -> s {reservedInstancesIds = a} :: ReservedInstancesModification) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML ReservedInstancesModification where
  parseXML x =
    ReservedInstancesModification'
      Prelude.<$> ( x Core..@? "modificationResultSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "status")
      Prelude.<*> (x Core..@? "clientToken")
      Prelude.<*> (x Core..@? "updateDate")
      Prelude.<*> (x Core..@? "createDate")
      Prelude.<*> (x Core..@? "effectiveDate")
      Prelude.<*> (x Core..@? "statusMessage")
      Prelude.<*> (x Core..@? "reservedInstancesModificationId")
      Prelude.<*> ( x Core..@? "reservedInstancesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    ReservedInstancesModification
  where
  hashWithSalt salt' ReservedInstancesModification' {..} =
    salt' `Prelude.hashWithSalt` reservedInstancesIds
      `Prelude.hashWithSalt` reservedInstancesModificationId
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` effectiveDate
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` updateDate
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` modificationResults

instance Prelude.NFData ReservedInstancesModification where
  rnf ReservedInstancesModification' {..} =
    Prelude.rnf modificationResults
      `Prelude.seq` Prelude.rnf reservedInstancesIds
      `Prelude.seq` Prelude.rnf reservedInstancesModificationId
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf effectiveDate
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf updateDate
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf status
