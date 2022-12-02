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
-- Module      : Amazonka.DirectConnect.DescribeConnections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the specified connection or all connections in this Region.
module Amazonka.DirectConnect.DescribeConnections
  ( -- * Creating a Request
    DescribeConnections (..),
    newDescribeConnections,

    -- * Request Lenses
    describeConnections_connectionId,

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

-- | /See:/ 'newDescribeConnections' smart constructor.
data DescribeConnections = DescribeConnections'
  { -- | The ID of the connection.
    connectionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionId', 'describeConnections_connectionId' - The ID of the connection.
newDescribeConnections ::
  DescribeConnections
newDescribeConnections =
  DescribeConnections'
    { connectionId =
        Prelude.Nothing
    }

-- | The ID of the connection.
describeConnections_connectionId :: Lens.Lens' DescribeConnections (Prelude.Maybe Prelude.Text)
describeConnections_connectionId = Lens.lens (\DescribeConnections' {connectionId} -> connectionId) (\s@DescribeConnections' {} a -> s {connectionId = a} :: DescribeConnections)

instance Core.AWSRequest DescribeConnections where
  type AWSResponse DescribeConnections = Connections
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DescribeConnections where
  hashWithSalt _salt DescribeConnections' {..} =
    _salt `Prelude.hashWithSalt` connectionId

instance Prelude.NFData DescribeConnections where
  rnf DescribeConnections' {..} =
    Prelude.rnf connectionId

instance Data.ToHeaders DescribeConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DescribeConnections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeConnections where
  toJSON DescribeConnections' {..} =
    Data.object
      ( Prelude.catMaybes
          [("connectionId" Data..=) Prelude.<$> connectionId]
      )

instance Data.ToPath DescribeConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeConnections where
  toQuery = Prelude.const Prelude.mempty
