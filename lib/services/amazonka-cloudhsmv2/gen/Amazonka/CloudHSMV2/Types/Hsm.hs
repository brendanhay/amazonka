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
-- Module      : Amazonka.CloudHSMV2.Types.Hsm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudHSMV2.Types.Hsm where

import Amazonka.CloudHSMV2.Types.HsmState
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a hardware security module (HSM) in an AWS
-- CloudHSM cluster.
--
-- /See:/ 'newHsm' smart constructor.
data Hsm = Hsm'
  { -- | The Availability Zone that contains the HSM.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The identifier (ID) of the cluster that contains the HSM.
    clusterId :: Prelude.Maybe Prelude.Text,
    -- | The identifier (ID) of the HSM\'s elastic network interface (ENI).
    eniId :: Prelude.Maybe Prelude.Text,
    -- | The IP address of the HSM\'s elastic network interface (ENI).
    eniIp :: Prelude.Maybe Prelude.Text,
    -- | The HSM\'s state.
    state :: Prelude.Maybe HsmState,
    -- | A description of the HSM\'s state.
    stateMessage :: Prelude.Maybe Prelude.Text,
    -- | The subnet that contains the HSM\'s elastic network interface (ENI).
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The HSM\'s identifier (ID).
    hsmId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Hsm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZone', 'hsm_availabilityZone' - The Availability Zone that contains the HSM.
--
-- 'clusterId', 'hsm_clusterId' - The identifier (ID) of the cluster that contains the HSM.
--
-- 'eniId', 'hsm_eniId' - The identifier (ID) of the HSM\'s elastic network interface (ENI).
--
-- 'eniIp', 'hsm_eniIp' - The IP address of the HSM\'s elastic network interface (ENI).
--
-- 'state', 'hsm_state' - The HSM\'s state.
--
-- 'stateMessage', 'hsm_stateMessage' - A description of the HSM\'s state.
--
-- 'subnetId', 'hsm_subnetId' - The subnet that contains the HSM\'s elastic network interface (ENI).
--
-- 'hsmId', 'hsm_hsmId' - The HSM\'s identifier (ID).
newHsm ::
  -- | 'hsmId'
  Prelude.Text ->
  Hsm
newHsm pHsmId_ =
  Hsm'
    { availabilityZone = Prelude.Nothing,
      clusterId = Prelude.Nothing,
      eniId = Prelude.Nothing,
      eniIp = Prelude.Nothing,
      state = Prelude.Nothing,
      stateMessage = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      hsmId = pHsmId_
    }

-- | The Availability Zone that contains the HSM.
hsm_availabilityZone :: Lens.Lens' Hsm (Prelude.Maybe Prelude.Text)
hsm_availabilityZone = Lens.lens (\Hsm' {availabilityZone} -> availabilityZone) (\s@Hsm' {} a -> s {availabilityZone = a} :: Hsm)

-- | The identifier (ID) of the cluster that contains the HSM.
hsm_clusterId :: Lens.Lens' Hsm (Prelude.Maybe Prelude.Text)
hsm_clusterId = Lens.lens (\Hsm' {clusterId} -> clusterId) (\s@Hsm' {} a -> s {clusterId = a} :: Hsm)

-- | The identifier (ID) of the HSM\'s elastic network interface (ENI).
hsm_eniId :: Lens.Lens' Hsm (Prelude.Maybe Prelude.Text)
hsm_eniId = Lens.lens (\Hsm' {eniId} -> eniId) (\s@Hsm' {} a -> s {eniId = a} :: Hsm)

-- | The IP address of the HSM\'s elastic network interface (ENI).
hsm_eniIp :: Lens.Lens' Hsm (Prelude.Maybe Prelude.Text)
hsm_eniIp = Lens.lens (\Hsm' {eniIp} -> eniIp) (\s@Hsm' {} a -> s {eniIp = a} :: Hsm)

-- | The HSM\'s state.
hsm_state :: Lens.Lens' Hsm (Prelude.Maybe HsmState)
hsm_state = Lens.lens (\Hsm' {state} -> state) (\s@Hsm' {} a -> s {state = a} :: Hsm)

-- | A description of the HSM\'s state.
hsm_stateMessage :: Lens.Lens' Hsm (Prelude.Maybe Prelude.Text)
hsm_stateMessage = Lens.lens (\Hsm' {stateMessage} -> stateMessage) (\s@Hsm' {} a -> s {stateMessage = a} :: Hsm)

-- | The subnet that contains the HSM\'s elastic network interface (ENI).
hsm_subnetId :: Lens.Lens' Hsm (Prelude.Maybe Prelude.Text)
hsm_subnetId = Lens.lens (\Hsm' {subnetId} -> subnetId) (\s@Hsm' {} a -> s {subnetId = a} :: Hsm)

-- | The HSM\'s identifier (ID).
hsm_hsmId :: Lens.Lens' Hsm Prelude.Text
hsm_hsmId = Lens.lens (\Hsm' {hsmId} -> hsmId) (\s@Hsm' {} a -> s {hsmId = a} :: Hsm)

instance Data.FromJSON Hsm where
  parseJSON =
    Data.withObject
      "Hsm"
      ( \x ->
          Hsm'
            Prelude.<$> (x Data..:? "AvailabilityZone")
            Prelude.<*> (x Data..:? "ClusterId")
            Prelude.<*> (x Data..:? "EniId")
            Prelude.<*> (x Data..:? "EniIp")
            Prelude.<*> (x Data..:? "State")
            Prelude.<*> (x Data..:? "StateMessage")
            Prelude.<*> (x Data..:? "SubnetId")
            Prelude.<*> (x Data..: "HsmId")
      )

instance Prelude.Hashable Hsm where
  hashWithSalt _salt Hsm' {..} =
    _salt
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` clusterId
      `Prelude.hashWithSalt` eniId
      `Prelude.hashWithSalt` eniIp
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` stateMessage
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` hsmId

instance Prelude.NFData Hsm where
  rnf Hsm' {..} =
    Prelude.rnf availabilityZone `Prelude.seq`
      Prelude.rnf clusterId `Prelude.seq`
        Prelude.rnf eniId `Prelude.seq`
          Prelude.rnf eniIp `Prelude.seq`
            Prelude.rnf state `Prelude.seq`
              Prelude.rnf stateMessage `Prelude.seq`
                Prelude.rnf subnetId `Prelude.seq`
                  Prelude.rnf hsmId
