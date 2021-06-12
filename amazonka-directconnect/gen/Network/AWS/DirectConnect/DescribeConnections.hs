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
-- Module      : Network.AWS.DirectConnect.DescribeConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays the specified connection or all connections in this Region.
module Network.AWS.DirectConnect.DescribeConnections
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeConnections' smart constructor.
data DescribeConnections = DescribeConnections'
  { -- | The ID of the connection.
    connectionId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  DescribeConnections' {connectionId = Core.Nothing}

-- | The ID of the connection.
describeConnections_connectionId :: Lens.Lens' DescribeConnections (Core.Maybe Core.Text)
describeConnections_connectionId = Lens.lens (\DescribeConnections' {connectionId} -> connectionId) (\s@DescribeConnections' {} a -> s {connectionId = a} :: DescribeConnections)

instance Core.AWSRequest DescribeConnections where
  type AWSResponse DescribeConnections = Connections
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable DescribeConnections

instance Core.NFData DescribeConnections

instance Core.ToHeaders DescribeConnections where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeConnections" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeConnections where
  toJSON DescribeConnections' {..} =
    Core.object
      ( Core.catMaybes
          [("connectionId" Core..=) Core.<$> connectionId]
      )

instance Core.ToPath DescribeConnections where
  toPath = Core.const "/"

instance Core.ToQuery DescribeConnections where
  toQuery = Core.const Core.mempty
