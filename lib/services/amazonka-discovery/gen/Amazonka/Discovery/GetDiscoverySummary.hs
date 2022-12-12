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
-- Module      : Amazonka.Discovery.GetDiscoverySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a short summary of discovered assets.
--
-- This API operation takes no request parameters and is called as is at
-- the command prompt as shown in the example.
module Amazonka.Discovery.GetDiscoverySummary
  ( -- * Creating a Request
    GetDiscoverySummary (..),
    newGetDiscoverySummary,

    -- * Destructuring the Response
    GetDiscoverySummaryResponse (..),
    newGetDiscoverySummaryResponse,

    -- * Response Lenses
    getDiscoverySummaryResponse_agentSummary,
    getDiscoverySummaryResponse_agentlessCollectorSummary,
    getDiscoverySummaryResponse_applications,
    getDiscoverySummaryResponse_connectorSummary,
    getDiscoverySummaryResponse_meCollectorSummary,
    getDiscoverySummaryResponse_servers,
    getDiscoverySummaryResponse_serversMappedToApplications,
    getDiscoverySummaryResponse_serversMappedtoTags,
    getDiscoverySummaryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Discovery.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDiscoverySummary' smart constructor.
data GetDiscoverySummary = GetDiscoverySummary'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDiscoverySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDiscoverySummary ::
  GetDiscoverySummary
newGetDiscoverySummary = GetDiscoverySummary'

instance Core.AWSRequest GetDiscoverySummary where
  type
    AWSResponse GetDiscoverySummary =
      GetDiscoverySummaryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDiscoverySummaryResponse'
            Prelude.<$> (x Data..?> "agentSummary")
            Prelude.<*> (x Data..?> "agentlessCollectorSummary")
            Prelude.<*> (x Data..?> "applications")
            Prelude.<*> (x Data..?> "connectorSummary")
            Prelude.<*> (x Data..?> "meCollectorSummary")
            Prelude.<*> (x Data..?> "servers")
            Prelude.<*> (x Data..?> "serversMappedToApplications")
            Prelude.<*> (x Data..?> "serversMappedtoTags")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDiscoverySummary where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetDiscoverySummary where
  rnf _ = ()

instance Data.ToHeaders GetDiscoverySummary where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPoseidonService_V2015_11_01.GetDiscoverySummary" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDiscoverySummary where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetDiscoverySummary where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDiscoverySummary where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDiscoverySummaryResponse' smart constructor.
data GetDiscoverySummaryResponse = GetDiscoverySummaryResponse'
  { -- | Details about discovered agents, including agent status and health.
    agentSummary :: Prelude.Maybe CustomerAgentInfo,
    agentlessCollectorSummary :: Prelude.Maybe CustomerAgentlessCollectorInfo,
    -- | The number of applications discovered.
    applications :: Prelude.Maybe Prelude.Integer,
    -- | Details about discovered connectors, including connector status and
    -- health.
    connectorSummary :: Prelude.Maybe CustomerConnectorInfo,
    -- | Details about Migration Evaluator collectors, including collector status
    -- and health.
    meCollectorSummary :: Prelude.Maybe CustomerMeCollectorInfo,
    -- | The number of servers discovered.
    servers :: Prelude.Maybe Prelude.Integer,
    -- | The number of servers mapped to applications.
    serversMappedToApplications :: Prelude.Maybe Prelude.Integer,
    -- | The number of servers mapped to tags.
    serversMappedtoTags :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDiscoverySummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentSummary', 'getDiscoverySummaryResponse_agentSummary' - Details about discovered agents, including agent status and health.
--
-- 'agentlessCollectorSummary', 'getDiscoverySummaryResponse_agentlessCollectorSummary' - Undocumented member.
--
-- 'applications', 'getDiscoverySummaryResponse_applications' - The number of applications discovered.
--
-- 'connectorSummary', 'getDiscoverySummaryResponse_connectorSummary' - Details about discovered connectors, including connector status and
-- health.
--
-- 'meCollectorSummary', 'getDiscoverySummaryResponse_meCollectorSummary' - Details about Migration Evaluator collectors, including collector status
-- and health.
--
-- 'servers', 'getDiscoverySummaryResponse_servers' - The number of servers discovered.
--
-- 'serversMappedToApplications', 'getDiscoverySummaryResponse_serversMappedToApplications' - The number of servers mapped to applications.
--
-- 'serversMappedtoTags', 'getDiscoverySummaryResponse_serversMappedtoTags' - The number of servers mapped to tags.
--
-- 'httpStatus', 'getDiscoverySummaryResponse_httpStatus' - The response's http status code.
newGetDiscoverySummaryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDiscoverySummaryResponse
newGetDiscoverySummaryResponse pHttpStatus_ =
  GetDiscoverySummaryResponse'
    { agentSummary =
        Prelude.Nothing,
      agentlessCollectorSummary = Prelude.Nothing,
      applications = Prelude.Nothing,
      connectorSummary = Prelude.Nothing,
      meCollectorSummary = Prelude.Nothing,
      servers = Prelude.Nothing,
      serversMappedToApplications = Prelude.Nothing,
      serversMappedtoTags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about discovered agents, including agent status and health.
getDiscoverySummaryResponse_agentSummary :: Lens.Lens' GetDiscoverySummaryResponse (Prelude.Maybe CustomerAgentInfo)
getDiscoverySummaryResponse_agentSummary = Lens.lens (\GetDiscoverySummaryResponse' {agentSummary} -> agentSummary) (\s@GetDiscoverySummaryResponse' {} a -> s {agentSummary = a} :: GetDiscoverySummaryResponse)

-- | Undocumented member.
getDiscoverySummaryResponse_agentlessCollectorSummary :: Lens.Lens' GetDiscoverySummaryResponse (Prelude.Maybe CustomerAgentlessCollectorInfo)
getDiscoverySummaryResponse_agentlessCollectorSummary = Lens.lens (\GetDiscoverySummaryResponse' {agentlessCollectorSummary} -> agentlessCollectorSummary) (\s@GetDiscoverySummaryResponse' {} a -> s {agentlessCollectorSummary = a} :: GetDiscoverySummaryResponse)

-- | The number of applications discovered.
getDiscoverySummaryResponse_applications :: Lens.Lens' GetDiscoverySummaryResponse (Prelude.Maybe Prelude.Integer)
getDiscoverySummaryResponse_applications = Lens.lens (\GetDiscoverySummaryResponse' {applications} -> applications) (\s@GetDiscoverySummaryResponse' {} a -> s {applications = a} :: GetDiscoverySummaryResponse)

-- | Details about discovered connectors, including connector status and
-- health.
getDiscoverySummaryResponse_connectorSummary :: Lens.Lens' GetDiscoverySummaryResponse (Prelude.Maybe CustomerConnectorInfo)
getDiscoverySummaryResponse_connectorSummary = Lens.lens (\GetDiscoverySummaryResponse' {connectorSummary} -> connectorSummary) (\s@GetDiscoverySummaryResponse' {} a -> s {connectorSummary = a} :: GetDiscoverySummaryResponse)

-- | Details about Migration Evaluator collectors, including collector status
-- and health.
getDiscoverySummaryResponse_meCollectorSummary :: Lens.Lens' GetDiscoverySummaryResponse (Prelude.Maybe CustomerMeCollectorInfo)
getDiscoverySummaryResponse_meCollectorSummary = Lens.lens (\GetDiscoverySummaryResponse' {meCollectorSummary} -> meCollectorSummary) (\s@GetDiscoverySummaryResponse' {} a -> s {meCollectorSummary = a} :: GetDiscoverySummaryResponse)

-- | The number of servers discovered.
getDiscoverySummaryResponse_servers :: Lens.Lens' GetDiscoverySummaryResponse (Prelude.Maybe Prelude.Integer)
getDiscoverySummaryResponse_servers = Lens.lens (\GetDiscoverySummaryResponse' {servers} -> servers) (\s@GetDiscoverySummaryResponse' {} a -> s {servers = a} :: GetDiscoverySummaryResponse)

-- | The number of servers mapped to applications.
getDiscoverySummaryResponse_serversMappedToApplications :: Lens.Lens' GetDiscoverySummaryResponse (Prelude.Maybe Prelude.Integer)
getDiscoverySummaryResponse_serversMappedToApplications = Lens.lens (\GetDiscoverySummaryResponse' {serversMappedToApplications} -> serversMappedToApplications) (\s@GetDiscoverySummaryResponse' {} a -> s {serversMappedToApplications = a} :: GetDiscoverySummaryResponse)

-- | The number of servers mapped to tags.
getDiscoverySummaryResponse_serversMappedtoTags :: Lens.Lens' GetDiscoverySummaryResponse (Prelude.Maybe Prelude.Integer)
getDiscoverySummaryResponse_serversMappedtoTags = Lens.lens (\GetDiscoverySummaryResponse' {serversMappedtoTags} -> serversMappedtoTags) (\s@GetDiscoverySummaryResponse' {} a -> s {serversMappedtoTags = a} :: GetDiscoverySummaryResponse)

-- | The response's http status code.
getDiscoverySummaryResponse_httpStatus :: Lens.Lens' GetDiscoverySummaryResponse Prelude.Int
getDiscoverySummaryResponse_httpStatus = Lens.lens (\GetDiscoverySummaryResponse' {httpStatus} -> httpStatus) (\s@GetDiscoverySummaryResponse' {} a -> s {httpStatus = a} :: GetDiscoverySummaryResponse)

instance Prelude.NFData GetDiscoverySummaryResponse where
  rnf GetDiscoverySummaryResponse' {..} =
    Prelude.rnf agentSummary
      `Prelude.seq` Prelude.rnf agentlessCollectorSummary
      `Prelude.seq` Prelude.rnf applications
      `Prelude.seq` Prelude.rnf connectorSummary
      `Prelude.seq` Prelude.rnf meCollectorSummary
      `Prelude.seq` Prelude.rnf servers
      `Prelude.seq` Prelude.rnf serversMappedToApplications
      `Prelude.seq` Prelude.rnf serversMappedtoTags
      `Prelude.seq` Prelude.rnf httpStatus
