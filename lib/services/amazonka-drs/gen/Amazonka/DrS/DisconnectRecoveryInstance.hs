{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DrS.DisconnectRecoveryInstance
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnect a Recovery Instance from Elastic Disaster Recovery. Data
-- replication is stopped immediately. All AWS resources created by Elastic
-- Disaster Recovery for enabling the replication of the Recovery Instance
-- will be terminated \/ deleted within 90 minutes. If the agent on the
-- Recovery Instance has not been prevented from communicating with the
-- Elastic Disaster Recovery service, then it will receive a command to
-- uninstall itself (within approximately 10 minutes). The following
-- properties of the Recovery Instance will be changed immediately:
-- dataReplicationInfo.dataReplicationState will be set to DISCONNECTED;
-- The totalStorageBytes property for each of
-- dataReplicationInfo.replicatedDisks will be set to zero;
-- dataReplicationInfo.lagDuration and dataReplicationInfo.lagDuration will
-- be nullified.
module Amazonka.DrS.DisconnectRecoveryInstance
  ( -- * Creating a Request
    DisconnectRecoveryInstance (..),
    newDisconnectRecoveryInstance,

    -- * Request Lenses
    disconnectRecoveryInstance_recoveryInstanceID,

    -- * Destructuring the Response
    DisconnectRecoveryInstanceResponse (..),
    newDisconnectRecoveryInstanceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisconnectRecoveryInstance' smart constructor.
data DisconnectRecoveryInstance = DisconnectRecoveryInstance'
  { -- | The ID of the Recovery Instance to disconnect.
    recoveryInstanceID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectRecoveryInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recoveryInstanceID', 'disconnectRecoveryInstance_recoveryInstanceID' - The ID of the Recovery Instance to disconnect.
newDisconnectRecoveryInstance ::
  -- | 'recoveryInstanceID'
  Prelude.Text ->
  DisconnectRecoveryInstance
newDisconnectRecoveryInstance pRecoveryInstanceID_ =
  DisconnectRecoveryInstance'
    { recoveryInstanceID =
        pRecoveryInstanceID_
    }

-- | The ID of the Recovery Instance to disconnect.
disconnectRecoveryInstance_recoveryInstanceID :: Lens.Lens' DisconnectRecoveryInstance Prelude.Text
disconnectRecoveryInstance_recoveryInstanceID = Lens.lens (\DisconnectRecoveryInstance' {recoveryInstanceID} -> recoveryInstanceID) (\s@DisconnectRecoveryInstance' {} a -> s {recoveryInstanceID = a} :: DisconnectRecoveryInstance)

instance Core.AWSRequest DisconnectRecoveryInstance where
  type
    AWSResponse DisconnectRecoveryInstance =
      DisconnectRecoveryInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DisconnectRecoveryInstanceResponse'

instance Prelude.Hashable DisconnectRecoveryInstance where
  hashWithSalt _salt DisconnectRecoveryInstance' {..} =
    _salt `Prelude.hashWithSalt` recoveryInstanceID

instance Prelude.NFData DisconnectRecoveryInstance where
  rnf DisconnectRecoveryInstance' {..} =
    Prelude.rnf recoveryInstanceID

instance Core.ToHeaders DisconnectRecoveryInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DisconnectRecoveryInstance where
  toJSON DisconnectRecoveryInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("recoveryInstanceID" Core..= recoveryInstanceID)
          ]
      )

instance Core.ToPath DisconnectRecoveryInstance where
  toPath = Prelude.const "/DisconnectRecoveryInstance"

instance Core.ToQuery DisconnectRecoveryInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisconnectRecoveryInstanceResponse' smart constructor.
data DisconnectRecoveryInstanceResponse = DisconnectRecoveryInstanceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectRecoveryInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisconnectRecoveryInstanceResponse ::
  DisconnectRecoveryInstanceResponse
newDisconnectRecoveryInstanceResponse =
  DisconnectRecoveryInstanceResponse'

instance
  Prelude.NFData
    DisconnectRecoveryInstanceResponse
  where
  rnf _ = ()
