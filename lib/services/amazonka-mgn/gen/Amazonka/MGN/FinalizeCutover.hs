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
-- Module      : Amazonka.MGN.FinalizeCutover
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Finalizes the cutover immediately for specific Source Servers. All AWS
-- resources created by Application Migration Service for enabling the
-- replication of these source servers will be terminated \/ deleted within
-- 90 minutes. Launched Test or Cutover instances will NOT be terminated.
-- The AWS Replication Agent will receive a command to uninstall itself
-- (within 10 minutes). The following properties of the SourceServer will
-- be changed immediately: dataReplicationInfo.dataReplicationState will be
-- changed to DISCONNECTED; The SourceServer.lifeCycle.state will be
-- changed to CUTOVER; The totalStorageBytes property fo each of
-- dataReplicationInfo.replicatedDisks will be set to zero;
-- dataReplicationInfo.lagDuration and dataReplicationInfo.lagDuration will
-- be nullified.
module Amazonka.MGN.FinalizeCutover
  ( -- * Creating a Request
    FinalizeCutover (..),
    newFinalizeCutover,

    -- * Request Lenses
    finalizeCutover_sourceServerID,

    -- * Destructuring the Response
    SourceServer (..),
    newSourceServer,

    -- * Response Lenses
    sourceServer_tags,
    sourceServer_vcenterClientID,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_launchedInstance,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_isArchived,
    sourceServer_sourceServerID,
    sourceServer_sourceProperties,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newFinalizeCutover' smart constructor.
data FinalizeCutover = FinalizeCutover'
  { -- | Request to finalize Cutover by Source Server ID.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FinalizeCutover' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServerID', 'finalizeCutover_sourceServerID' - Request to finalize Cutover by Source Server ID.
newFinalizeCutover ::
  -- | 'sourceServerID'
  Prelude.Text ->
  FinalizeCutover
newFinalizeCutover pSourceServerID_ =
  FinalizeCutover' {sourceServerID = pSourceServerID_}

-- | Request to finalize Cutover by Source Server ID.
finalizeCutover_sourceServerID :: Lens.Lens' FinalizeCutover Prelude.Text
finalizeCutover_sourceServerID = Lens.lens (\FinalizeCutover' {sourceServerID} -> sourceServerID) (\s@FinalizeCutover' {} a -> s {sourceServerID = a} :: FinalizeCutover)

instance Core.AWSRequest FinalizeCutover where
  type AWSResponse FinalizeCutover = SourceServer
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable FinalizeCutover where
  hashWithSalt _salt FinalizeCutover' {..} =
    _salt `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData FinalizeCutover where
  rnf FinalizeCutover' {..} = Prelude.rnf sourceServerID

instance Data.ToHeaders FinalizeCutover where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON FinalizeCutover where
  toJSON FinalizeCutover' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath FinalizeCutover where
  toPath = Prelude.const "/FinalizeCutover"

instance Data.ToQuery FinalizeCutover where
  toQuery = Prelude.const Prelude.mempty
