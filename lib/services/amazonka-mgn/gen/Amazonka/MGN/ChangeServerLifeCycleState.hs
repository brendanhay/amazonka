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
-- Module      : Amazonka.MGN.ChangeServerLifeCycleState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the user to set the SourceServer.LifeCycle.state property for
-- specific Source Server IDs to one of the following: READY_FOR_TEST or
-- READY_FOR_CUTOVER. This command only works if the Source Server is
-- already launchable (dataReplicationInfo.lagDuration is not null.)
module Amazonka.MGN.ChangeServerLifeCycleState
  ( -- * Creating a Request
    ChangeServerLifeCycleState (..),
    newChangeServerLifeCycleState,

    -- * Request Lenses
    changeServerLifeCycleState_lifeCycle,
    changeServerLifeCycleState_sourceServerID,

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

-- | /See:/ 'newChangeServerLifeCycleState' smart constructor.
data ChangeServerLifeCycleState = ChangeServerLifeCycleState'
  { -- | The request to change the source server migration lifecycle state.
    lifeCycle :: ChangeServerLifeCycleStateSourceServerLifecycle,
    -- | The request to change the source server migration lifecycle state by
    -- source server ID.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeServerLifeCycleState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifeCycle', 'changeServerLifeCycleState_lifeCycle' - The request to change the source server migration lifecycle state.
--
-- 'sourceServerID', 'changeServerLifeCycleState_sourceServerID' - The request to change the source server migration lifecycle state by
-- source server ID.
newChangeServerLifeCycleState ::
  -- | 'lifeCycle'
  ChangeServerLifeCycleStateSourceServerLifecycle ->
  -- | 'sourceServerID'
  Prelude.Text ->
  ChangeServerLifeCycleState
newChangeServerLifeCycleState
  pLifeCycle_
  pSourceServerID_ =
    ChangeServerLifeCycleState'
      { lifeCycle =
          pLifeCycle_,
        sourceServerID = pSourceServerID_
      }

-- | The request to change the source server migration lifecycle state.
changeServerLifeCycleState_lifeCycle :: Lens.Lens' ChangeServerLifeCycleState ChangeServerLifeCycleStateSourceServerLifecycle
changeServerLifeCycleState_lifeCycle = Lens.lens (\ChangeServerLifeCycleState' {lifeCycle} -> lifeCycle) (\s@ChangeServerLifeCycleState' {} a -> s {lifeCycle = a} :: ChangeServerLifeCycleState)

-- | The request to change the source server migration lifecycle state by
-- source server ID.
changeServerLifeCycleState_sourceServerID :: Lens.Lens' ChangeServerLifeCycleState Prelude.Text
changeServerLifeCycleState_sourceServerID = Lens.lens (\ChangeServerLifeCycleState' {sourceServerID} -> sourceServerID) (\s@ChangeServerLifeCycleState' {} a -> s {sourceServerID = a} :: ChangeServerLifeCycleState)

instance Core.AWSRequest ChangeServerLifeCycleState where
  type
    AWSResponse ChangeServerLifeCycleState =
      SourceServer
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable ChangeServerLifeCycleState where
  hashWithSalt _salt ChangeServerLifeCycleState' {..} =
    _salt
      `Prelude.hashWithSalt` lifeCycle
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData ChangeServerLifeCycleState where
  rnf ChangeServerLifeCycleState' {..} =
    Prelude.rnf lifeCycle
      `Prelude.seq` Prelude.rnf sourceServerID

instance Data.ToHeaders ChangeServerLifeCycleState where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ChangeServerLifeCycleState where
  toJSON ChangeServerLifeCycleState' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("lifeCycle" Data..= lifeCycle),
            Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath ChangeServerLifeCycleState where
  toPath = Prelude.const "/ChangeServerLifeCycleState"

instance Data.ToQuery ChangeServerLifeCycleState where
  toQuery = Prelude.const Prelude.mempty
