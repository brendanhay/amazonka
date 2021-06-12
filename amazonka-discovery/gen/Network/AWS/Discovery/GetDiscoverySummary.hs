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
-- Module      : Network.AWS.Discovery.GetDiscoverySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a short summary of discovered assets.
--
-- This API operation takes no request parameters and is called as is at
-- the command prompt as shown in the example.
module Network.AWS.Discovery.GetDiscoverySummary
  ( -- * Creating a Request
    GetDiscoverySummary (..),
    newGetDiscoverySummary,

    -- * Destructuring the Response
    GetDiscoverySummaryResponse (..),
    newGetDiscoverySummaryResponse,

    -- * Response Lenses
    getDiscoverySummaryResponse_servers,
    getDiscoverySummaryResponse_agentSummary,
    getDiscoverySummaryResponse_connectorSummary,
    getDiscoverySummaryResponse_serversMappedToApplications,
    getDiscoverySummaryResponse_applications,
    getDiscoverySummaryResponse_serversMappedtoTags,
    getDiscoverySummaryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Discovery.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDiscoverySummary' smart constructor.
data GetDiscoverySummary = GetDiscoverySummary'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDiscoverySummaryResponse'
            Core.<$> (x Core..?> "servers")
            Core.<*> (x Core..?> "agentSummary")
            Core.<*> (x Core..?> "connectorSummary")
            Core.<*> (x Core..?> "serversMappedToApplications")
            Core.<*> (x Core..?> "applications")
            Core.<*> (x Core..?> "serversMappedtoTags")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDiscoverySummary

instance Core.NFData GetDiscoverySummary

instance Core.ToHeaders GetDiscoverySummary where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSPoseidonService_V2015_11_01.GetDiscoverySummary" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDiscoverySummary where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetDiscoverySummary where
  toPath = Core.const "/"

instance Core.ToQuery GetDiscoverySummary where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDiscoverySummaryResponse' smart constructor.
data GetDiscoverySummaryResponse = GetDiscoverySummaryResponse'
  { -- | The number of servers discovered.
    servers :: Core.Maybe Core.Integer,
    -- | Details about discovered agents, including agent status and health.
    agentSummary :: Core.Maybe CustomerAgentInfo,
    -- | Details about discovered connectors, including connector status and
    -- health.
    connectorSummary :: Core.Maybe CustomerConnectorInfo,
    -- | The number of servers mapped to applications.
    serversMappedToApplications :: Core.Maybe Core.Integer,
    -- | The number of applications discovered.
    applications :: Core.Maybe Core.Integer,
    -- | The number of servers mapped to tags.
    serversMappedtoTags :: Core.Maybe Core.Integer,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDiscoverySummaryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'servers', 'getDiscoverySummaryResponse_servers' - The number of servers discovered.
--
-- 'agentSummary', 'getDiscoverySummaryResponse_agentSummary' - Details about discovered agents, including agent status and health.
--
-- 'connectorSummary', 'getDiscoverySummaryResponse_connectorSummary' - Details about discovered connectors, including connector status and
-- health.
--
-- 'serversMappedToApplications', 'getDiscoverySummaryResponse_serversMappedToApplications' - The number of servers mapped to applications.
--
-- 'applications', 'getDiscoverySummaryResponse_applications' - The number of applications discovered.
--
-- 'serversMappedtoTags', 'getDiscoverySummaryResponse_serversMappedtoTags' - The number of servers mapped to tags.
--
-- 'httpStatus', 'getDiscoverySummaryResponse_httpStatus' - The response's http status code.
newGetDiscoverySummaryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDiscoverySummaryResponse
newGetDiscoverySummaryResponse pHttpStatus_ =
  GetDiscoverySummaryResponse'
    { servers =
        Core.Nothing,
      agentSummary = Core.Nothing,
      connectorSummary = Core.Nothing,
      serversMappedToApplications = Core.Nothing,
      applications = Core.Nothing,
      serversMappedtoTags = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of servers discovered.
getDiscoverySummaryResponse_servers :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe Core.Integer)
getDiscoverySummaryResponse_servers = Lens.lens (\GetDiscoverySummaryResponse' {servers} -> servers) (\s@GetDiscoverySummaryResponse' {} a -> s {servers = a} :: GetDiscoverySummaryResponse)

-- | Details about discovered agents, including agent status and health.
getDiscoverySummaryResponse_agentSummary :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe CustomerAgentInfo)
getDiscoverySummaryResponse_agentSummary = Lens.lens (\GetDiscoverySummaryResponse' {agentSummary} -> agentSummary) (\s@GetDiscoverySummaryResponse' {} a -> s {agentSummary = a} :: GetDiscoverySummaryResponse)

-- | Details about discovered connectors, including connector status and
-- health.
getDiscoverySummaryResponse_connectorSummary :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe CustomerConnectorInfo)
getDiscoverySummaryResponse_connectorSummary = Lens.lens (\GetDiscoverySummaryResponse' {connectorSummary} -> connectorSummary) (\s@GetDiscoverySummaryResponse' {} a -> s {connectorSummary = a} :: GetDiscoverySummaryResponse)

-- | The number of servers mapped to applications.
getDiscoverySummaryResponse_serversMappedToApplications :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe Core.Integer)
getDiscoverySummaryResponse_serversMappedToApplications = Lens.lens (\GetDiscoverySummaryResponse' {serversMappedToApplications} -> serversMappedToApplications) (\s@GetDiscoverySummaryResponse' {} a -> s {serversMappedToApplications = a} :: GetDiscoverySummaryResponse)

-- | The number of applications discovered.
getDiscoverySummaryResponse_applications :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe Core.Integer)
getDiscoverySummaryResponse_applications = Lens.lens (\GetDiscoverySummaryResponse' {applications} -> applications) (\s@GetDiscoverySummaryResponse' {} a -> s {applications = a} :: GetDiscoverySummaryResponse)

-- | The number of servers mapped to tags.
getDiscoverySummaryResponse_serversMappedtoTags :: Lens.Lens' GetDiscoverySummaryResponse (Core.Maybe Core.Integer)
getDiscoverySummaryResponse_serversMappedtoTags = Lens.lens (\GetDiscoverySummaryResponse' {serversMappedtoTags} -> serversMappedtoTags) (\s@GetDiscoverySummaryResponse' {} a -> s {serversMappedtoTags = a} :: GetDiscoverySummaryResponse)

-- | The response's http status code.
getDiscoverySummaryResponse_httpStatus :: Lens.Lens' GetDiscoverySummaryResponse Core.Int
getDiscoverySummaryResponse_httpStatus = Lens.lens (\GetDiscoverySummaryResponse' {httpStatus} -> httpStatus) (\s@GetDiscoverySummaryResponse' {} a -> s {httpStatus = a} :: GetDiscoverySummaryResponse)

instance Core.NFData GetDiscoverySummaryResponse
