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
-- Module      : Amazonka.Kafka.Types.VpcConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.VpcConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.VpcConnectionState
import qualified Amazonka.Prelude as Prelude

-- | The VPC connection object.
--
-- /See:/ 'newVpcConnection' smart constructor.
data VpcConnection = VpcConnection'
  { -- | Information about the auth scheme of Vpc Connection.
    authentication :: Prelude.Maybe Prelude.Text,
    -- | Creation time of the Vpc Connection.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | State of the Vpc Connection.
    state :: Prelude.Maybe VpcConnectionState,
    -- | The vpcId that belongs to the Vpc Connection.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ARN that identifies the Vpc Connection.
    vpcConnectionArn :: Prelude.Text,
    -- | The ARN that identifies the Cluster which the Vpc Connection belongs to.
    targetClusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authentication', 'vpcConnection_authentication' - Information about the auth scheme of Vpc Connection.
--
-- 'creationTime', 'vpcConnection_creationTime' - Creation time of the Vpc Connection.
--
-- 'state', 'vpcConnection_state' - State of the Vpc Connection.
--
-- 'vpcId', 'vpcConnection_vpcId' - The vpcId that belongs to the Vpc Connection.
--
-- 'vpcConnectionArn', 'vpcConnection_vpcConnectionArn' - The ARN that identifies the Vpc Connection.
--
-- 'targetClusterArn', 'vpcConnection_targetClusterArn' - The ARN that identifies the Cluster which the Vpc Connection belongs to.
newVpcConnection ::
  -- | 'vpcConnectionArn'
  Prelude.Text ->
  -- | 'targetClusterArn'
  Prelude.Text ->
  VpcConnection
newVpcConnection
  pVpcConnectionArn_
  pTargetClusterArn_ =
    VpcConnection'
      { authentication = Prelude.Nothing,
        creationTime = Prelude.Nothing,
        state = Prelude.Nothing,
        vpcId = Prelude.Nothing,
        vpcConnectionArn = pVpcConnectionArn_,
        targetClusterArn = pTargetClusterArn_
      }

-- | Information about the auth scheme of Vpc Connection.
vpcConnection_authentication :: Lens.Lens' VpcConnection (Prelude.Maybe Prelude.Text)
vpcConnection_authentication = Lens.lens (\VpcConnection' {authentication} -> authentication) (\s@VpcConnection' {} a -> s {authentication = a} :: VpcConnection)

-- | Creation time of the Vpc Connection.
vpcConnection_creationTime :: Lens.Lens' VpcConnection (Prelude.Maybe Prelude.UTCTime)
vpcConnection_creationTime = Lens.lens (\VpcConnection' {creationTime} -> creationTime) (\s@VpcConnection' {} a -> s {creationTime = a} :: VpcConnection) Prelude.. Lens.mapping Data._Time

-- | State of the Vpc Connection.
vpcConnection_state :: Lens.Lens' VpcConnection (Prelude.Maybe VpcConnectionState)
vpcConnection_state = Lens.lens (\VpcConnection' {state} -> state) (\s@VpcConnection' {} a -> s {state = a} :: VpcConnection)

-- | The vpcId that belongs to the Vpc Connection.
vpcConnection_vpcId :: Lens.Lens' VpcConnection (Prelude.Maybe Prelude.Text)
vpcConnection_vpcId = Lens.lens (\VpcConnection' {vpcId} -> vpcId) (\s@VpcConnection' {} a -> s {vpcId = a} :: VpcConnection)

-- | The ARN that identifies the Vpc Connection.
vpcConnection_vpcConnectionArn :: Lens.Lens' VpcConnection Prelude.Text
vpcConnection_vpcConnectionArn = Lens.lens (\VpcConnection' {vpcConnectionArn} -> vpcConnectionArn) (\s@VpcConnection' {} a -> s {vpcConnectionArn = a} :: VpcConnection)

-- | The ARN that identifies the Cluster which the Vpc Connection belongs to.
vpcConnection_targetClusterArn :: Lens.Lens' VpcConnection Prelude.Text
vpcConnection_targetClusterArn = Lens.lens (\VpcConnection' {targetClusterArn} -> targetClusterArn) (\s@VpcConnection' {} a -> s {targetClusterArn = a} :: VpcConnection)

instance Data.FromJSON VpcConnection where
  parseJSON =
    Data.withObject
      "VpcConnection"
      ( \x ->
          VpcConnection'
            Prelude.<$> (x Data..:? "authentication")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "vpcId")
            Prelude.<*> (x Data..: "vpcConnectionArn")
            Prelude.<*> (x Data..: "targetClusterArn")
      )

instance Prelude.Hashable VpcConnection where
  hashWithSalt _salt VpcConnection' {..} =
    _salt
      `Prelude.hashWithSalt` authentication
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` vpcConnectionArn
      `Prelude.hashWithSalt` targetClusterArn

instance Prelude.NFData VpcConnection where
  rnf VpcConnection' {..} =
    Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf vpcConnectionArn
      `Prelude.seq` Prelude.rnf targetClusterArn
