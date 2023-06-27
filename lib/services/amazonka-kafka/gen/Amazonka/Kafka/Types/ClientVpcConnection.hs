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
-- Module      : Amazonka.Kafka.Types.ClientVpcConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ClientVpcConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types.VpcConnectionState
import qualified Amazonka.Prelude as Prelude

-- | The client VPC connection object.
--
-- /See:/ 'newClientVpcConnection' smart constructor.
data ClientVpcConnection = ClientVpcConnection'
  { -- | Information about the auth scheme of Vpc Connection.
    authentication :: Prelude.Maybe Prelude.Text,
    -- | Creation time of the Vpc Connection.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The Owner of the Vpc Connection.
    owner :: Prelude.Maybe Prelude.Text,
    -- | State of the Vpc Connection.
    state :: Prelude.Maybe VpcConnectionState,
    -- | The ARN that identifies the Vpc Connection.
    vpcConnectionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientVpcConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authentication', 'clientVpcConnection_authentication' - Information about the auth scheme of Vpc Connection.
--
-- 'creationTime', 'clientVpcConnection_creationTime' - Creation time of the Vpc Connection.
--
-- 'owner', 'clientVpcConnection_owner' - The Owner of the Vpc Connection.
--
-- 'state', 'clientVpcConnection_state' - State of the Vpc Connection.
--
-- 'vpcConnectionArn', 'clientVpcConnection_vpcConnectionArn' - The ARN that identifies the Vpc Connection.
newClientVpcConnection ::
  -- | 'vpcConnectionArn'
  Prelude.Text ->
  ClientVpcConnection
newClientVpcConnection pVpcConnectionArn_ =
  ClientVpcConnection'
    { authentication =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      owner = Prelude.Nothing,
      state = Prelude.Nothing,
      vpcConnectionArn = pVpcConnectionArn_
    }

-- | Information about the auth scheme of Vpc Connection.
clientVpcConnection_authentication :: Lens.Lens' ClientVpcConnection (Prelude.Maybe Prelude.Text)
clientVpcConnection_authentication = Lens.lens (\ClientVpcConnection' {authentication} -> authentication) (\s@ClientVpcConnection' {} a -> s {authentication = a} :: ClientVpcConnection)

-- | Creation time of the Vpc Connection.
clientVpcConnection_creationTime :: Lens.Lens' ClientVpcConnection (Prelude.Maybe Prelude.UTCTime)
clientVpcConnection_creationTime = Lens.lens (\ClientVpcConnection' {creationTime} -> creationTime) (\s@ClientVpcConnection' {} a -> s {creationTime = a} :: ClientVpcConnection) Prelude.. Lens.mapping Data._Time

-- | The Owner of the Vpc Connection.
clientVpcConnection_owner :: Lens.Lens' ClientVpcConnection (Prelude.Maybe Prelude.Text)
clientVpcConnection_owner = Lens.lens (\ClientVpcConnection' {owner} -> owner) (\s@ClientVpcConnection' {} a -> s {owner = a} :: ClientVpcConnection)

-- | State of the Vpc Connection.
clientVpcConnection_state :: Lens.Lens' ClientVpcConnection (Prelude.Maybe VpcConnectionState)
clientVpcConnection_state = Lens.lens (\ClientVpcConnection' {state} -> state) (\s@ClientVpcConnection' {} a -> s {state = a} :: ClientVpcConnection)

-- | The ARN that identifies the Vpc Connection.
clientVpcConnection_vpcConnectionArn :: Lens.Lens' ClientVpcConnection Prelude.Text
clientVpcConnection_vpcConnectionArn = Lens.lens (\ClientVpcConnection' {vpcConnectionArn} -> vpcConnectionArn) (\s@ClientVpcConnection' {} a -> s {vpcConnectionArn = a} :: ClientVpcConnection)

instance Data.FromJSON ClientVpcConnection where
  parseJSON =
    Data.withObject
      "ClientVpcConnection"
      ( \x ->
          ClientVpcConnection'
            Prelude.<$> (x Data..:? "authentication")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..: "vpcConnectionArn")
      )

instance Prelude.Hashable ClientVpcConnection where
  hashWithSalt _salt ClientVpcConnection' {..} =
    _salt
      `Prelude.hashWithSalt` authentication
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` vpcConnectionArn

instance Prelude.NFData ClientVpcConnection where
  rnf ClientVpcConnection' {..} =
    Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf vpcConnectionArn
