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
-- Module      : Network.AWS.OpsWorksCM.DescribeServers
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all configuration management servers that are identified with your
-- account. Only the stored results from Amazon DynamoDB are returned. AWS
-- OpsWorks CM does not query other services.
--
-- This operation is synchronous.
--
-- A @ResourceNotFoundException@ is thrown when the server does not exist.
-- A @ValidationException@ is raised when parameters of the request are not
-- valid.
--
-- This operation returns paginated results.
module Network.AWS.OpsWorksCM.DescribeServers
  ( -- * Creating a Request
    DescribeServers (..),
    newDescribeServers,

    -- * Request Lenses
    describeServers_nextToken,
    describeServers_maxResults,
    describeServers_serverName,

    -- * Destructuring the Response
    DescribeServersResponse (..),
    newDescribeServersResponse,

    -- * Response Lenses
    describeServersResponse_nextToken,
    describeServersResponse_servers,
    describeServersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorksCM.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeServers' smart constructor.
data DescribeServers = DescribeServers'
  { -- | This is not currently implemented for @DescribeServers@ requests.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This is not currently implemented for @DescribeServers@ requests.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Describes the server with the specified ServerName.
    serverName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeServers_nextToken' - This is not currently implemented for @DescribeServers@ requests.
--
-- 'maxResults', 'describeServers_maxResults' - This is not currently implemented for @DescribeServers@ requests.
--
-- 'serverName', 'describeServers_serverName' - Describes the server with the specified ServerName.
newDescribeServers ::
  DescribeServers
newDescribeServers =
  DescribeServers'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      serverName = Prelude.Nothing
    }

-- | This is not currently implemented for @DescribeServers@ requests.
describeServers_nextToken :: Lens.Lens' DescribeServers (Prelude.Maybe Prelude.Text)
describeServers_nextToken = Lens.lens (\DescribeServers' {nextToken} -> nextToken) (\s@DescribeServers' {} a -> s {nextToken = a} :: DescribeServers)

-- | This is not currently implemented for @DescribeServers@ requests.
describeServers_maxResults :: Lens.Lens' DescribeServers (Prelude.Maybe Prelude.Natural)
describeServers_maxResults = Lens.lens (\DescribeServers' {maxResults} -> maxResults) (\s@DescribeServers' {} a -> s {maxResults = a} :: DescribeServers)

-- | Describes the server with the specified ServerName.
describeServers_serverName :: Lens.Lens' DescribeServers (Prelude.Maybe Prelude.Text)
describeServers_serverName = Lens.lens (\DescribeServers' {serverName} -> serverName) (\s@DescribeServers' {} a -> s {serverName = a} :: DescribeServers)

instance Core.AWSPager DescribeServers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeServersResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeServersResponse_servers Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeServers_nextToken
          Lens..~ rs
          Lens.^? describeServersResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeServers where
  type
    AWSResponse DescribeServers =
      DescribeServersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServersResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Servers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServers

instance Prelude.NFData DescribeServers

instance Core.ToHeaders DescribeServers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorksCM_V2016_11_01.DescribeServers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeServers where
  toJSON DescribeServers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("ServerName" Core..=) Prelude.<$> serverName
          ]
      )

instance Core.ToPath DescribeServers where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeServers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServersResponse' smart constructor.
data DescribeServersResponse = DescribeServersResponse'
  { -- | This is not currently implemented for @DescribeServers@ requests.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains the response to a @DescribeServers@ request.
    --
    -- /For Chef Automate servers:/ If
    -- @DescribeServersResponse$Servers$EngineAttributes@ includes
    -- CHEF_MAJOR_UPGRADE_AVAILABLE, you can upgrade the Chef Automate server
    -- to Chef Automate 2. To be eligible for upgrade, a server running Chef
    -- Automate 1 must have had at least one successful maintenance run after
    -- November 1, 2019.
    --
    -- /For Puppet Server:/ @DescribeServersResponse$Servers$EngineAttributes@
    -- contains PUPPET_API_CA_CERT. This is the PEM-encoded CA certificate that
    -- is used by the Puppet API over TCP port number 8140. The CA certificate
    -- is also used to sign node certificates.
    servers :: Prelude.Maybe [Server],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeServersResponse_nextToken' - This is not currently implemented for @DescribeServers@ requests.
--
-- 'servers', 'describeServersResponse_servers' - Contains the response to a @DescribeServers@ request.
--
-- /For Chef Automate servers:/ If
-- @DescribeServersResponse$Servers$EngineAttributes@ includes
-- CHEF_MAJOR_UPGRADE_AVAILABLE, you can upgrade the Chef Automate server
-- to Chef Automate 2. To be eligible for upgrade, a server running Chef
-- Automate 1 must have had at least one successful maintenance run after
-- November 1, 2019.
--
-- /For Puppet Server:/ @DescribeServersResponse$Servers$EngineAttributes@
-- contains PUPPET_API_CA_CERT. This is the PEM-encoded CA certificate that
-- is used by the Puppet API over TCP port number 8140. The CA certificate
-- is also used to sign node certificates.
--
-- 'httpStatus', 'describeServersResponse_httpStatus' - The response's http status code.
newDescribeServersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServersResponse
newDescribeServersResponse pHttpStatus_ =
  DescribeServersResponse'
    { nextToken =
        Prelude.Nothing,
      servers = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | This is not currently implemented for @DescribeServers@ requests.
describeServersResponse_nextToken :: Lens.Lens' DescribeServersResponse (Prelude.Maybe Prelude.Text)
describeServersResponse_nextToken = Lens.lens (\DescribeServersResponse' {nextToken} -> nextToken) (\s@DescribeServersResponse' {} a -> s {nextToken = a} :: DescribeServersResponse)

-- | Contains the response to a @DescribeServers@ request.
--
-- /For Chef Automate servers:/ If
-- @DescribeServersResponse$Servers$EngineAttributes@ includes
-- CHEF_MAJOR_UPGRADE_AVAILABLE, you can upgrade the Chef Automate server
-- to Chef Automate 2. To be eligible for upgrade, a server running Chef
-- Automate 1 must have had at least one successful maintenance run after
-- November 1, 2019.
--
-- /For Puppet Server:/ @DescribeServersResponse$Servers$EngineAttributes@
-- contains PUPPET_API_CA_CERT. This is the PEM-encoded CA certificate that
-- is used by the Puppet API over TCP port number 8140. The CA certificate
-- is also used to sign node certificates.
describeServersResponse_servers :: Lens.Lens' DescribeServersResponse (Prelude.Maybe [Server])
describeServersResponse_servers = Lens.lens (\DescribeServersResponse' {servers} -> servers) (\s@DescribeServersResponse' {} a -> s {servers = a} :: DescribeServersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeServersResponse_httpStatus :: Lens.Lens' DescribeServersResponse Prelude.Int
describeServersResponse_httpStatus = Lens.lens (\DescribeServersResponse' {httpStatus} -> httpStatus) (\s@DescribeServersResponse' {} a -> s {httpStatus = a} :: DescribeServersResponse)

instance Prelude.NFData DescribeServersResponse
