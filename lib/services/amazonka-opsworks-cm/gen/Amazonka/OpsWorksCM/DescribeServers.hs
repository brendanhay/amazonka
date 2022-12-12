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
-- Module      : Amazonka.OpsWorksCM.DescribeServers
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.OpsWorksCM.DescribeServers
  ( -- * Creating a Request
    DescribeServers (..),
    newDescribeServers,

    -- * Request Lenses
    describeServers_maxResults,
    describeServers_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorksCM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeServers' smart constructor.
data DescribeServers = DescribeServers'
  { -- | This is not currently implemented for @DescribeServers@ requests.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | This is not currently implemented for @DescribeServers@ requests.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'maxResults', 'describeServers_maxResults' - This is not currently implemented for @DescribeServers@ requests.
--
-- 'nextToken', 'describeServers_nextToken' - This is not currently implemented for @DescribeServers@ requests.
--
-- 'serverName', 'describeServers_serverName' - Describes the server with the specified ServerName.
newDescribeServers ::
  DescribeServers
newDescribeServers =
  DescribeServers'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serverName = Prelude.Nothing
    }

-- | This is not currently implemented for @DescribeServers@ requests.
describeServers_maxResults :: Lens.Lens' DescribeServers (Prelude.Maybe Prelude.Natural)
describeServers_maxResults = Lens.lens (\DescribeServers' {maxResults} -> maxResults) (\s@DescribeServers' {} a -> s {maxResults = a} :: DescribeServers)

-- | This is not currently implemented for @DescribeServers@ requests.
describeServers_nextToken :: Lens.Lens' DescribeServers (Prelude.Maybe Prelude.Text)
describeServers_nextToken = Lens.lens (\DescribeServers' {nextToken} -> nextToken) (\s@DescribeServers' {} a -> s {nextToken = a} :: DescribeServers)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServersResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Servers" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServers where
  hashWithSalt _salt DescribeServers' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serverName

instance Prelude.NFData DescribeServers where
  rnf DescribeServers' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serverName

instance Data.ToHeaders DescribeServers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorksCM_V2016_11_01.DescribeServers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeServers where
  toJSON DescribeServers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("ServerName" Data..=) Prelude.<$> serverName
          ]
      )

instance Data.ToPath DescribeServers where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeServers where
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
    -- /For Puppet servers:/ @DescribeServersResponse$Servers$EngineAttributes@
    -- contains the following two responses:
    --
    -- -   @PUPPET_API_CA_CERT@, the PEM-encoded CA certificate that is used by
    --     the Puppet API over TCP port number 8140. The CA certificate is also
    --     used to sign node certificates.
    --
    -- -   @PUPPET_API_CRL@, a certificate revocation list. The certificate
    --     revocation list is for internal maintenance purposes only. For more
    --     information about the Puppet certificate revocation list, see
    --     <https://puppet.com/docs/puppet/5.5/man/certificate_revocation_list.html Man Page: puppet certificate_revocation_list>
    --     in the Puppet documentation.
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
-- /For Puppet servers:/ @DescribeServersResponse$Servers$EngineAttributes@
-- contains the following two responses:
--
-- -   @PUPPET_API_CA_CERT@, the PEM-encoded CA certificate that is used by
--     the Puppet API over TCP port number 8140. The CA certificate is also
--     used to sign node certificates.
--
-- -   @PUPPET_API_CRL@, a certificate revocation list. The certificate
--     revocation list is for internal maintenance purposes only. For more
--     information about the Puppet certificate revocation list, see
--     <https://puppet.com/docs/puppet/5.5/man/certificate_revocation_list.html Man Page: puppet certificate_revocation_list>
--     in the Puppet documentation.
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
-- /For Puppet servers:/ @DescribeServersResponse$Servers$EngineAttributes@
-- contains the following two responses:
--
-- -   @PUPPET_API_CA_CERT@, the PEM-encoded CA certificate that is used by
--     the Puppet API over TCP port number 8140. The CA certificate is also
--     used to sign node certificates.
--
-- -   @PUPPET_API_CRL@, a certificate revocation list. The certificate
--     revocation list is for internal maintenance purposes only. For more
--     information about the Puppet certificate revocation list, see
--     <https://puppet.com/docs/puppet/5.5/man/certificate_revocation_list.html Man Page: puppet certificate_revocation_list>
--     in the Puppet documentation.
describeServersResponse_servers :: Lens.Lens' DescribeServersResponse (Prelude.Maybe [Server])
describeServersResponse_servers = Lens.lens (\DescribeServersResponse' {servers} -> servers) (\s@DescribeServersResponse' {} a -> s {servers = a} :: DescribeServersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeServersResponse_httpStatus :: Lens.Lens' DescribeServersResponse Prelude.Int
describeServersResponse_httpStatus = Lens.lens (\DescribeServersResponse' {httpStatus} -> httpStatus) (\s@DescribeServersResponse' {} a -> s {httpStatus = a} :: DescribeServersResponse)

instance Prelude.NFData DescribeServersResponse where
  rnf DescribeServersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf servers
      `Prelude.seq` Prelude.rnf httpStatus
