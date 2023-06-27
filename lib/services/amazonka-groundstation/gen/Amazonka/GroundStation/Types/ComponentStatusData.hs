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
-- Module      : Amazonka.GroundStation.Types.ComponentStatusData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ComponentStatusData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.AgentStatus
import qualified Amazonka.Prelude as Prelude

-- | Data on the status of agent components.
--
-- /See:/ 'newComponentStatusData' smart constructor.
data ComponentStatusData = ComponentStatusData'
  { -- | Bytes received by the component.
    bytesReceived :: Prelude.Maybe Prelude.Integer,
    -- | Bytes sent by the component.
    bytesSent :: Prelude.Maybe Prelude.Integer,
    -- | Packets dropped by component.
    packetsDropped :: Prelude.Maybe Prelude.Integer,
    -- | Capability ARN of the component.
    capabilityArn :: Prelude.Text,
    -- | The Component type.
    componentType :: Prelude.Text,
    -- | Dataflow UUID associated with the component.
    dataflowId :: Prelude.Text,
    -- | Component status.
    status :: AgentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComponentStatusData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytesReceived', 'componentStatusData_bytesReceived' - Bytes received by the component.
--
-- 'bytesSent', 'componentStatusData_bytesSent' - Bytes sent by the component.
--
-- 'packetsDropped', 'componentStatusData_packetsDropped' - Packets dropped by component.
--
-- 'capabilityArn', 'componentStatusData_capabilityArn' - Capability ARN of the component.
--
-- 'componentType', 'componentStatusData_componentType' - The Component type.
--
-- 'dataflowId', 'componentStatusData_dataflowId' - Dataflow UUID associated with the component.
--
-- 'status', 'componentStatusData_status' - Component status.
newComponentStatusData ::
  -- | 'capabilityArn'
  Prelude.Text ->
  -- | 'componentType'
  Prelude.Text ->
  -- | 'dataflowId'
  Prelude.Text ->
  -- | 'status'
  AgentStatus ->
  ComponentStatusData
newComponentStatusData
  pCapabilityArn_
  pComponentType_
  pDataflowId_
  pStatus_ =
    ComponentStatusData'
      { bytesReceived =
          Prelude.Nothing,
        bytesSent = Prelude.Nothing,
        packetsDropped = Prelude.Nothing,
        capabilityArn = pCapabilityArn_,
        componentType = pComponentType_,
        dataflowId = pDataflowId_,
        status = pStatus_
      }

-- | Bytes received by the component.
componentStatusData_bytesReceived :: Lens.Lens' ComponentStatusData (Prelude.Maybe Prelude.Integer)
componentStatusData_bytesReceived = Lens.lens (\ComponentStatusData' {bytesReceived} -> bytesReceived) (\s@ComponentStatusData' {} a -> s {bytesReceived = a} :: ComponentStatusData)

-- | Bytes sent by the component.
componentStatusData_bytesSent :: Lens.Lens' ComponentStatusData (Prelude.Maybe Prelude.Integer)
componentStatusData_bytesSent = Lens.lens (\ComponentStatusData' {bytesSent} -> bytesSent) (\s@ComponentStatusData' {} a -> s {bytesSent = a} :: ComponentStatusData)

-- | Packets dropped by component.
componentStatusData_packetsDropped :: Lens.Lens' ComponentStatusData (Prelude.Maybe Prelude.Integer)
componentStatusData_packetsDropped = Lens.lens (\ComponentStatusData' {packetsDropped} -> packetsDropped) (\s@ComponentStatusData' {} a -> s {packetsDropped = a} :: ComponentStatusData)

-- | Capability ARN of the component.
componentStatusData_capabilityArn :: Lens.Lens' ComponentStatusData Prelude.Text
componentStatusData_capabilityArn = Lens.lens (\ComponentStatusData' {capabilityArn} -> capabilityArn) (\s@ComponentStatusData' {} a -> s {capabilityArn = a} :: ComponentStatusData)

-- | The Component type.
componentStatusData_componentType :: Lens.Lens' ComponentStatusData Prelude.Text
componentStatusData_componentType = Lens.lens (\ComponentStatusData' {componentType} -> componentType) (\s@ComponentStatusData' {} a -> s {componentType = a} :: ComponentStatusData)

-- | Dataflow UUID associated with the component.
componentStatusData_dataflowId :: Lens.Lens' ComponentStatusData Prelude.Text
componentStatusData_dataflowId = Lens.lens (\ComponentStatusData' {dataflowId} -> dataflowId) (\s@ComponentStatusData' {} a -> s {dataflowId = a} :: ComponentStatusData)

-- | Component status.
componentStatusData_status :: Lens.Lens' ComponentStatusData AgentStatus
componentStatusData_status = Lens.lens (\ComponentStatusData' {status} -> status) (\s@ComponentStatusData' {} a -> s {status = a} :: ComponentStatusData)

instance Prelude.Hashable ComponentStatusData where
  hashWithSalt _salt ComponentStatusData' {..} =
    _salt
      `Prelude.hashWithSalt` bytesReceived
      `Prelude.hashWithSalt` bytesSent
      `Prelude.hashWithSalt` packetsDropped
      `Prelude.hashWithSalt` capabilityArn
      `Prelude.hashWithSalt` componentType
      `Prelude.hashWithSalt` dataflowId
      `Prelude.hashWithSalt` status

instance Prelude.NFData ComponentStatusData where
  rnf ComponentStatusData' {..} =
    Prelude.rnf bytesReceived
      `Prelude.seq` Prelude.rnf bytesSent
      `Prelude.seq` Prelude.rnf packetsDropped
      `Prelude.seq` Prelude.rnf capabilityArn
      `Prelude.seq` Prelude.rnf componentType
      `Prelude.seq` Prelude.rnf dataflowId
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON ComponentStatusData where
  toJSON ComponentStatusData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("bytesReceived" Data..=) Prelude.<$> bytesReceived,
            ("bytesSent" Data..=) Prelude.<$> bytesSent,
            ("packetsDropped" Data..=)
              Prelude.<$> packetsDropped,
            Prelude.Just ("capabilityArn" Data..= capabilityArn),
            Prelude.Just ("componentType" Data..= componentType),
            Prelude.Just ("dataflowId" Data..= dataflowId),
            Prelude.Just ("status" Data..= status)
          ]
      )
