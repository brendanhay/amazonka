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
-- Module      : Amazonka.MGN.DisconnectFromService
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disconnects specific Source Servers from Application Migration Service.
-- Data replication is stopped immediately. All AWS resources created by
-- Application Migration Service for enabling the replication of these
-- source servers will be terminated \/ deleted within 90 minutes. Launched
-- Test or Cutover instances will NOT be terminated. If the agent on the
-- source server has not been prevented from communicating with the
-- Application Migration Service service, then it will receive a command to
-- uninstall itself (within approximately 10 minutes). The following
-- properties of the SourceServer will be changed immediately:
-- dataReplicationInfo.dataReplicationState will be set to DISCONNECTED;
-- The totalStorageBytes property for each of
-- dataReplicationInfo.replicatedDisks will be set to zero;
-- dataReplicationInfo.lagDuration and dataReplicationInfo.lagDuration will
-- be nullified.
module Amazonka.MGN.DisconnectFromService
  ( -- * Creating a Request
    DisconnectFromService (..),
    newDisconnectFromService,

    -- * Request Lenses
    disconnectFromService_sourceServerID,

    -- * Destructuring the Response
    SourceServer (..),
    newSourceServer,

    -- * Response Lenses
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_fqdnForActionFramework,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
    sourceServer_userProvidedID,
    sourceServer_vcenterClientID,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisconnectFromService' smart constructor.
data DisconnectFromService = DisconnectFromService'
  { -- | Request to disconnect Source Server from service by Server ID.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisconnectFromService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServerID', 'disconnectFromService_sourceServerID' - Request to disconnect Source Server from service by Server ID.
newDisconnectFromService ::
  -- | 'sourceServerID'
  Prelude.Text ->
  DisconnectFromService
newDisconnectFromService pSourceServerID_ =
  DisconnectFromService'
    { sourceServerID =
        pSourceServerID_
    }

-- | Request to disconnect Source Server from service by Server ID.
disconnectFromService_sourceServerID :: Lens.Lens' DisconnectFromService Prelude.Text
disconnectFromService_sourceServerID = Lens.lens (\DisconnectFromService' {sourceServerID} -> sourceServerID) (\s@DisconnectFromService' {} a -> s {sourceServerID = a} :: DisconnectFromService)

instance Core.AWSRequest DisconnectFromService where
  type AWSResponse DisconnectFromService = SourceServer
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DisconnectFromService where
  hashWithSalt _salt DisconnectFromService' {..} =
    _salt `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData DisconnectFromService where
  rnf DisconnectFromService' {..} =
    Prelude.rnf sourceServerID

instance Data.ToHeaders DisconnectFromService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisconnectFromService where
  toJSON DisconnectFromService' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath DisconnectFromService where
  toPath = Prelude.const "/DisconnectFromService"

instance Data.ToQuery DisconnectFromService where
  toQuery = Prelude.const Prelude.mempty
