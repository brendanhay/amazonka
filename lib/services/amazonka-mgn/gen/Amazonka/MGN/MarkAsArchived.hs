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
-- Module      : Amazonka.MGN.MarkAsArchived
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Archives specific Source Servers by setting the SourceServer.isArchived
-- property to true for specified SourceServers by ID. This command only
-- works for SourceServers with a lifecycle. state which equals
-- DISCONNECTED or CUTOVER.
module Amazonka.MGN.MarkAsArchived
  ( -- * Creating a Request
    MarkAsArchived (..),
    newMarkAsArchived,

    -- * Request Lenses
    markAsArchived_sourceServerID,

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

-- | /See:/ 'newMarkAsArchived' smart constructor.
data MarkAsArchived = MarkAsArchived'
  { -- | Mark as archived by Source Server ID.
    sourceServerID :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MarkAsArchived' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceServerID', 'markAsArchived_sourceServerID' - Mark as archived by Source Server ID.
newMarkAsArchived ::
  -- | 'sourceServerID'
  Prelude.Text ->
  MarkAsArchived
newMarkAsArchived pSourceServerID_ =
  MarkAsArchived' {sourceServerID = pSourceServerID_}

-- | Mark as archived by Source Server ID.
markAsArchived_sourceServerID :: Lens.Lens' MarkAsArchived Prelude.Text
markAsArchived_sourceServerID = Lens.lens (\MarkAsArchived' {sourceServerID} -> sourceServerID) (\s@MarkAsArchived' {} a -> s {sourceServerID = a} :: MarkAsArchived)

instance Core.AWSRequest MarkAsArchived where
  type AWSResponse MarkAsArchived = SourceServer
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable MarkAsArchived where
  hashWithSalt _salt MarkAsArchived' {..} =
    _salt `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData MarkAsArchived where
  rnf MarkAsArchived' {..} = Prelude.rnf sourceServerID

instance Data.ToHeaders MarkAsArchived where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON MarkAsArchived where
  toJSON MarkAsArchived' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("sourceServerID" Data..= sourceServerID)
          ]
      )

instance Data.ToPath MarkAsArchived where
  toPath = Prelude.const "/MarkAsArchived"

instance Data.ToQuery MarkAsArchived where
  toQuery = Prelude.const Prelude.mempty
