{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.DirectConnect.DescribeHostedConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the hosted connections that have been provisioned on the specified
-- interconnect or link aggregation group (LAG).
--
-- Intended for use by AWS Direct Connect Partners only.
module Network.AWS.DirectConnect.DescribeHostedConnections
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

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeHostedConnections' smart constructor.
data DescribeHostedConnections = DescribeHostedConnections'
  { -- | The ID of the interconnect or LAG.
    connectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeHostedConnections where
  type Rs DescribeHostedConnections = Connections
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable DescribeHostedConnections

instance Prelude.NFData DescribeHostedConnections

instance Prelude.ToHeaders DescribeHostedConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "OvertureService.DescribeHostedConnections" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeHostedConnections where
  toJSON DescribeHostedConnections' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("connectionId" Prelude..= connectionId)
          ]
      )

instance Prelude.ToPath DescribeHostedConnections where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeHostedConnections where
  toQuery = Prelude.const Prelude.mempty
