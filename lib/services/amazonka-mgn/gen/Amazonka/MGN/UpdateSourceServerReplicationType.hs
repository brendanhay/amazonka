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
-- Module      : Amazonka.MGN.UpdateSourceServerReplicationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to change between the AGENT_BASED replication type and the
-- SNAPSHOT_SHIPPING replication type.
module Amazonka.MGN.UpdateSourceServerReplicationType
  ( -- * Creating a Request
    UpdateSourceServerReplicationType (..),
    newUpdateSourceServerReplicationType,

    -- * Request Lenses
    updateSourceServerReplicationType_replicationType,
    updateSourceServerReplicationType_sourceServerID,

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

-- | /See:/ 'newUpdateSourceServerReplicationType' smart constructor.
data UpdateSourceServerReplicationType = UpdateSourceServerReplicationType'
  { -- | Replication type to which to update source server.
    replicationType :: ReplicationType,
    -- | ID of source server on which to update replication type.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSourceServerReplicationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationType', 'updateSourceServerReplicationType_replicationType' - Replication type to which to update source server.
--
-- 'sourceServerID', 'updateSourceServerReplicationType_sourceServerID' - ID of source server on which to update replication type.
newUpdateSourceServerReplicationType ::
  -- | 'replicationType'
  ReplicationType ->
  -- | 'sourceServerID'
  Prelude.Text ->
  UpdateSourceServerReplicationType
newUpdateSourceServerReplicationType
  pReplicationType_
  pSourceServerID_ =
    UpdateSourceServerReplicationType'
      { replicationType =
          pReplicationType_,
        sourceServerID = pSourceServerID_
      }

-- | Replication type to which to update source server.
updateSourceServerReplicationType_replicationType :: Lens.Lens' UpdateSourceServerReplicationType ReplicationType
updateSourceServerReplicationType_replicationType = Lens.lens (\UpdateSourceServerReplicationType' {replicationType} -> replicationType) (\s@UpdateSourceServerReplicationType' {} a -> s {replicationType = a} :: UpdateSourceServerReplicationType)

-- | ID of source server on which to update replication type.
updateSourceServerReplicationType_sourceServerID :: Lens.Lens' UpdateSourceServerReplicationType Prelude.Text
updateSourceServerReplicationType_sourceServerID = Lens.lens (\UpdateSourceServerReplicationType' {sourceServerID} -> sourceServerID) (\s@UpdateSourceServerReplicationType' {} a -> s {sourceServerID = a} :: UpdateSourceServerReplicationType)

instance
  Core.AWSRequest
    UpdateSourceServerReplicationType
  where
  type
    AWSResponse UpdateSourceServerReplicationType =
      SourceServer
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance
  Prelude.Hashable
    UpdateSourceServerReplicationType
  where
  hashWithSalt
    _salt
    UpdateSourceServerReplicationType' {..} =
      _salt `Prelude.hashWithSalt` replicationType
        `Prelude.hashWithSalt` sourceServerID

instance
  Prelude.NFData
    UpdateSourceServerReplicationType
  where
  rnf UpdateSourceServerReplicationType' {..} =
    Prelude.rnf replicationType
      `Prelude.seq` Prelude.rnf sourceServerID

instance
  Data.ToHeaders
    UpdateSourceServerReplicationType
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateSourceServerReplicationType
  where
  toJSON UpdateSourceServerReplicationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("replicationType" Data..= replicationType),
            Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance
  Data.ToPath
    UpdateSourceServerReplicationType
  where
  toPath =
    Prelude.const "/UpdateSourceServerReplicationType"

instance
  Data.ToQuery
    UpdateSourceServerReplicationType
  where
  toQuery = Prelude.const Prelude.mempty
