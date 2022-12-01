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
-- Module      : Amazonka.Route53RecoveryControlConfig.ListAssociatedRoute53HealthChecks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of all Amazon Route 53 health checks associated with a
-- specific routing control.
module Amazonka.Route53RecoveryControlConfig.ListAssociatedRoute53HealthChecks
  ( -- * Creating a Request
    ListAssociatedRoute53HealthChecks (..),
    newListAssociatedRoute53HealthChecks,

    -- * Request Lenses
    listAssociatedRoute53HealthChecks_nextToken,
    listAssociatedRoute53HealthChecks_maxResults,
    listAssociatedRoute53HealthChecks_routingControlArn,

    -- * Destructuring the Response
    ListAssociatedRoute53HealthChecksResponse (..),
    newListAssociatedRoute53HealthChecksResponse,

    -- * Response Lenses
    listAssociatedRoute53HealthChecksResponse_nextToken,
    listAssociatedRoute53HealthChecksResponse_healthCheckIds,
    listAssociatedRoute53HealthChecksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | /See:/ 'newListAssociatedRoute53HealthChecks' smart constructor.
data ListAssociatedRoute53HealthChecks = ListAssociatedRoute53HealthChecks'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the routing control.
    routingControlArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociatedRoute53HealthChecks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociatedRoute53HealthChecks_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'maxResults', 'listAssociatedRoute53HealthChecks_maxResults' - The number of objects that you want to return with this call.
--
-- 'routingControlArn', 'listAssociatedRoute53HealthChecks_routingControlArn' - The Amazon Resource Name (ARN) of the routing control.
newListAssociatedRoute53HealthChecks ::
  -- | 'routingControlArn'
  Prelude.Text ->
  ListAssociatedRoute53HealthChecks
newListAssociatedRoute53HealthChecks
  pRoutingControlArn_ =
    ListAssociatedRoute53HealthChecks'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        routingControlArn = pRoutingControlArn_
      }

-- | The token that identifies which batch of results you want to see.
listAssociatedRoute53HealthChecks_nextToken :: Lens.Lens' ListAssociatedRoute53HealthChecks (Prelude.Maybe Prelude.Text)
listAssociatedRoute53HealthChecks_nextToken = Lens.lens (\ListAssociatedRoute53HealthChecks' {nextToken} -> nextToken) (\s@ListAssociatedRoute53HealthChecks' {} a -> s {nextToken = a} :: ListAssociatedRoute53HealthChecks)

-- | The number of objects that you want to return with this call.
listAssociatedRoute53HealthChecks_maxResults :: Lens.Lens' ListAssociatedRoute53HealthChecks (Prelude.Maybe Prelude.Natural)
listAssociatedRoute53HealthChecks_maxResults = Lens.lens (\ListAssociatedRoute53HealthChecks' {maxResults} -> maxResults) (\s@ListAssociatedRoute53HealthChecks' {} a -> s {maxResults = a} :: ListAssociatedRoute53HealthChecks)

-- | The Amazon Resource Name (ARN) of the routing control.
listAssociatedRoute53HealthChecks_routingControlArn :: Lens.Lens' ListAssociatedRoute53HealthChecks Prelude.Text
listAssociatedRoute53HealthChecks_routingControlArn = Lens.lens (\ListAssociatedRoute53HealthChecks' {routingControlArn} -> routingControlArn) (\s@ListAssociatedRoute53HealthChecks' {} a -> s {routingControlArn = a} :: ListAssociatedRoute53HealthChecks)

instance
  Core.AWSRequest
    ListAssociatedRoute53HealthChecks
  where
  type
    AWSResponse ListAssociatedRoute53HealthChecks =
      ListAssociatedRoute53HealthChecksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssociatedRoute53HealthChecksResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> (x Core..?> "HealthCheckIds" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListAssociatedRoute53HealthChecks
  where
  hashWithSalt
    _salt
    ListAssociatedRoute53HealthChecks' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` routingControlArn

instance
  Prelude.NFData
    ListAssociatedRoute53HealthChecks
  where
  rnf ListAssociatedRoute53HealthChecks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf routingControlArn

instance
  Core.ToHeaders
    ListAssociatedRoute53HealthChecks
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToPath
    ListAssociatedRoute53HealthChecks
  where
  toPath ListAssociatedRoute53HealthChecks' {..} =
    Prelude.mconcat
      [ "/routingcontrol/",
        Core.toBS routingControlArn,
        "/associatedRoute53HealthChecks"
      ]

instance
  Core.ToQuery
    ListAssociatedRoute53HealthChecks
  where
  toQuery ListAssociatedRoute53HealthChecks' {..} =
    Prelude.mconcat
      [ "NextToken" Core.=: nextToken,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListAssociatedRoute53HealthChecksResponse' smart constructor.
data ListAssociatedRoute53HealthChecksResponse = ListAssociatedRoute53HealthChecksResponse'
  { -- | Next token for listing health checks.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Identifiers for the health checks.
    healthCheckIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAssociatedRoute53HealthChecksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAssociatedRoute53HealthChecksResponse_nextToken' - Next token for listing health checks.
--
-- 'healthCheckIds', 'listAssociatedRoute53HealthChecksResponse_healthCheckIds' - Identifiers for the health checks.
--
-- 'httpStatus', 'listAssociatedRoute53HealthChecksResponse_httpStatus' - The response's http status code.
newListAssociatedRoute53HealthChecksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAssociatedRoute53HealthChecksResponse
newListAssociatedRoute53HealthChecksResponse
  pHttpStatus_ =
    ListAssociatedRoute53HealthChecksResponse'
      { nextToken =
          Prelude.Nothing,
        healthCheckIds = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Next token for listing health checks.
listAssociatedRoute53HealthChecksResponse_nextToken :: Lens.Lens' ListAssociatedRoute53HealthChecksResponse (Prelude.Maybe Prelude.Text)
listAssociatedRoute53HealthChecksResponse_nextToken = Lens.lens (\ListAssociatedRoute53HealthChecksResponse' {nextToken} -> nextToken) (\s@ListAssociatedRoute53HealthChecksResponse' {} a -> s {nextToken = a} :: ListAssociatedRoute53HealthChecksResponse)

-- | Identifiers for the health checks.
listAssociatedRoute53HealthChecksResponse_healthCheckIds :: Lens.Lens' ListAssociatedRoute53HealthChecksResponse (Prelude.Maybe [Prelude.Text])
listAssociatedRoute53HealthChecksResponse_healthCheckIds = Lens.lens (\ListAssociatedRoute53HealthChecksResponse' {healthCheckIds} -> healthCheckIds) (\s@ListAssociatedRoute53HealthChecksResponse' {} a -> s {healthCheckIds = a} :: ListAssociatedRoute53HealthChecksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAssociatedRoute53HealthChecksResponse_httpStatus :: Lens.Lens' ListAssociatedRoute53HealthChecksResponse Prelude.Int
listAssociatedRoute53HealthChecksResponse_httpStatus = Lens.lens (\ListAssociatedRoute53HealthChecksResponse' {httpStatus} -> httpStatus) (\s@ListAssociatedRoute53HealthChecksResponse' {} a -> s {httpStatus = a} :: ListAssociatedRoute53HealthChecksResponse)

instance
  Prelude.NFData
    ListAssociatedRoute53HealthChecksResponse
  where
  rnf ListAssociatedRoute53HealthChecksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf healthCheckIds
      `Prelude.seq` Prelude.rnf httpStatus
