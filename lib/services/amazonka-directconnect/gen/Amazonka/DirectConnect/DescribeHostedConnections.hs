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
-- Module      : Amazonka.DirectConnect.DescribeHostedConnections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the hosted connections that have been provisioned on the specified
-- interconnect or link aggregation group (LAG).
--
-- Intended for use by Direct Connect Partners only.
module Amazonka.DirectConnect.DescribeHostedConnections
  ( -- * Creating a Request
    DescribeHostedConnections (..),
    newDescribeHostedConnections,

    -- * Request Lenses
    describeHostedConnections_connectionId,

    -- * Destructuring the Response
    Connections (..),
    newConnections,

    -- * Response Lenses
    connections_connections,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeHostedConnections' smart constructor.
data DescribeHostedConnections = DescribeHostedConnections'
  { -- | The ID of the interconnect or LAG.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeHostedConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'describeHostedConnections_connectionId' - The ID of the interconnect or LAG.
newDescribeHostedConnections ::
  -- | 'connectionId'
  Prelude.Text ->
  DescribeHostedConnections
newDescribeHostedConnections pConnectionId_ =
  DescribeHostedConnections'
    { connectionId =
        pConnectionId_
    }

-- | The ID of the interconnect or LAG.
describeHostedConnections_connectionId :: Lens.Lens' DescribeHostedConnections Prelude.Text
describeHostedConnections_connectionId = Lens.lens (\DescribeHostedConnections' {connectionId} -> connectionId) (\s@DescribeHostedConnections' {} a -> s {connectionId = a} :: DescribeHostedConnections)

instance Core.AWSRequest DescribeHostedConnections where
  type
    AWSResponse DescribeHostedConnections =
      Connections
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DescribeHostedConnections where
  hashWithSalt _salt DescribeHostedConnections' {..} =
    _salt `Prelude.hashWithSalt` connectionId

instance Prelude.NFData DescribeHostedConnections where
  rnf DescribeHostedConnections' {..} =
    Prelude.rnf connectionId

instance Data.ToHeaders DescribeHostedConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DescribeHostedConnections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeHostedConnections where
  toJSON DescribeHostedConnections' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("connectionId" Data..= connectionId)]
      )

instance Data.ToPath DescribeHostedConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeHostedConnections where
  toQuery = Prelude.const Prelude.mempty
