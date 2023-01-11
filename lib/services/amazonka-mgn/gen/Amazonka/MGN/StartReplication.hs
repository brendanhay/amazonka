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
-- Module      : Amazonka.MGN.StartReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts replication for SNAPSHOT_SHIPPING agents.
module Amazonka.MGN.StartReplication
  ( -- * Creating a Request
    StartReplication (..),
    newStartReplication,

    -- * Request Lenses
    startReplication_sourceServerID,

    -- * Destructuring the Response
    SourceServer (..),
    newSourceServer,

    -- * Response Lenses
    sourceServer_applicationID,
    sourceServer_arn,
    sourceServer_dataReplicationInfo,
    sourceServer_isArchived,
    sourceServer_launchedInstance,
    sourceServer_lifeCycle,
    sourceServer_replicationType,
    sourceServer_sourceProperties,
    sourceServer_sourceServerID,
    sourceServer_tags,
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

-- | /See:/ 'newStartReplication' smart constructor.
data StartReplication = StartReplication'
  { -- | ID of source server on which to start replication.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServerID', 'startReplication_sourceServerID' - ID of source server on which to start replication.
newStartReplication ::
  -- | 'sourceServerID'
  Prelude.Text ->
  StartReplication
newStartReplication pSourceServerID_ =
  StartReplication'
    { sourceServerID =
        pSourceServerID_
    }

-- | ID of source server on which to start replication.
startReplication_sourceServerID :: Lens.Lens' StartReplication Prelude.Text
startReplication_sourceServerID = Lens.lens (\StartReplication' {sourceServerID} -> sourceServerID) (\s@StartReplication' {} a -> s {sourceServerID = a} :: StartReplication)

instance Core.AWSRequest StartReplication where
  type AWSResponse StartReplication = SourceServer
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable StartReplication where
  hashWithSalt _salt StartReplication' {..} =
    _salt `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData StartReplication where
  rnf StartReplication' {..} =
    Prelude.rnf sourceServerID

instance Data.ToHeaders StartReplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartReplication where
  toJSON StartReplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath StartReplication where
  toPath = Prelude.const "/StartReplication"

instance Data.ToQuery StartReplication where
  toQuery = Prelude.const Prelude.mempty
