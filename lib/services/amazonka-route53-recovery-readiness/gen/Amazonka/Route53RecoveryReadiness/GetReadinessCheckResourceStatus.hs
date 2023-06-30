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
-- Module      : Amazonka.Route53RecoveryReadiness.GetReadinessCheckResourceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets individual readiness status for a readiness check. To see the
-- overall readiness status for a recovery group, that considers the
-- readiness status for all the readiness checks in the recovery group, use
-- GetRecoveryGroupReadinessSummary.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.GetReadinessCheckResourceStatus
  ( -- * Creating a Request
    GetReadinessCheckResourceStatus (..),
    newGetReadinessCheckResourceStatus,

    -- * Request Lenses
    getReadinessCheckResourceStatus_maxResults,
    getReadinessCheckResourceStatus_nextToken,
    getReadinessCheckResourceStatus_readinessCheckName,
    getReadinessCheckResourceStatus_resourceIdentifier,

    -- * Destructuring the Response
    GetReadinessCheckResourceStatusResponse (..),
    newGetReadinessCheckResourceStatusResponse,

    -- * Response Lenses
    getReadinessCheckResourceStatusResponse_nextToken,
    getReadinessCheckResourceStatusResponse_readiness,
    getReadinessCheckResourceStatusResponse_rules,
    getReadinessCheckResourceStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetReadinessCheckResourceStatus' smart constructor.
data GetReadinessCheckResourceStatus = GetReadinessCheckResourceStatus'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Name of a readiness check.
    readinessCheckName :: Prelude.Text,
    -- | The resource identifier, which is the Amazon Resource Name (ARN) or the
    -- identifier generated for the resource by Application Recovery Controller
    -- (for example, for a DNS target resource).
    resourceIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadinessCheckResourceStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getReadinessCheckResourceStatus_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'getReadinessCheckResourceStatus_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'readinessCheckName', 'getReadinessCheckResourceStatus_readinessCheckName' - Name of a readiness check.
--
-- 'resourceIdentifier', 'getReadinessCheckResourceStatus_resourceIdentifier' - The resource identifier, which is the Amazon Resource Name (ARN) or the
-- identifier generated for the resource by Application Recovery Controller
-- (for example, for a DNS target resource).
newGetReadinessCheckResourceStatus ::
  -- | 'readinessCheckName'
  Prelude.Text ->
  -- | 'resourceIdentifier'
  Prelude.Text ->
  GetReadinessCheckResourceStatus
newGetReadinessCheckResourceStatus
  pReadinessCheckName_
  pResourceIdentifier_ =
    GetReadinessCheckResourceStatus'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        readinessCheckName = pReadinessCheckName_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | The number of objects that you want to return with this call.
getReadinessCheckResourceStatus_maxResults :: Lens.Lens' GetReadinessCheckResourceStatus (Prelude.Maybe Prelude.Natural)
getReadinessCheckResourceStatus_maxResults = Lens.lens (\GetReadinessCheckResourceStatus' {maxResults} -> maxResults) (\s@GetReadinessCheckResourceStatus' {} a -> s {maxResults = a} :: GetReadinessCheckResourceStatus)

-- | The token that identifies which batch of results you want to see.
getReadinessCheckResourceStatus_nextToken :: Lens.Lens' GetReadinessCheckResourceStatus (Prelude.Maybe Prelude.Text)
getReadinessCheckResourceStatus_nextToken = Lens.lens (\GetReadinessCheckResourceStatus' {nextToken} -> nextToken) (\s@GetReadinessCheckResourceStatus' {} a -> s {nextToken = a} :: GetReadinessCheckResourceStatus)

-- | Name of a readiness check.
getReadinessCheckResourceStatus_readinessCheckName :: Lens.Lens' GetReadinessCheckResourceStatus Prelude.Text
getReadinessCheckResourceStatus_readinessCheckName = Lens.lens (\GetReadinessCheckResourceStatus' {readinessCheckName} -> readinessCheckName) (\s@GetReadinessCheckResourceStatus' {} a -> s {readinessCheckName = a} :: GetReadinessCheckResourceStatus)

-- | The resource identifier, which is the Amazon Resource Name (ARN) or the
-- identifier generated for the resource by Application Recovery Controller
-- (for example, for a DNS target resource).
getReadinessCheckResourceStatus_resourceIdentifier :: Lens.Lens' GetReadinessCheckResourceStatus Prelude.Text
getReadinessCheckResourceStatus_resourceIdentifier = Lens.lens (\GetReadinessCheckResourceStatus' {resourceIdentifier} -> resourceIdentifier) (\s@GetReadinessCheckResourceStatus' {} a -> s {resourceIdentifier = a} :: GetReadinessCheckResourceStatus)

instance
  Core.AWSPager
    GetReadinessCheckResourceStatus
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getReadinessCheckResourceStatusResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getReadinessCheckResourceStatusResponse_rules
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getReadinessCheckResourceStatus_nextToken
          Lens..~ rs
          Lens.^? getReadinessCheckResourceStatusResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetReadinessCheckResourceStatus
  where
  type
    AWSResponse GetReadinessCheckResourceStatus =
      GetReadinessCheckResourceStatusResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReadinessCheckResourceStatusResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "readiness")
            Prelude.<*> (x Data..?> "rules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetReadinessCheckResourceStatus
  where
  hashWithSalt
    _salt
    GetReadinessCheckResourceStatus' {..} =
      _salt
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` readinessCheckName
        `Prelude.hashWithSalt` resourceIdentifier

instance
  Prelude.NFData
    GetReadinessCheckResourceStatus
  where
  rnf GetReadinessCheckResourceStatus' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf readinessCheckName
      `Prelude.seq` Prelude.rnf resourceIdentifier

instance
  Data.ToHeaders
    GetReadinessCheckResourceStatus
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetReadinessCheckResourceStatus where
  toPath GetReadinessCheckResourceStatus' {..} =
    Prelude.mconcat
      [ "/readinesschecks/",
        Data.toBS readinessCheckName,
        "/resource/",
        Data.toBS resourceIdentifier,
        "/status"
      ]

instance Data.ToQuery GetReadinessCheckResourceStatus where
  toQuery GetReadinessCheckResourceStatus' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetReadinessCheckResourceStatusResponse' smart constructor.
data GetReadinessCheckResourceStatusResponse = GetReadinessCheckResourceStatusResponse'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The readiness at a rule level.
    readiness :: Prelude.Maybe Readiness,
    -- | Details of the rule\'s results.
    rules :: Prelude.Maybe [RuleResult],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadinessCheckResourceStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getReadinessCheckResourceStatusResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'readiness', 'getReadinessCheckResourceStatusResponse_readiness' - The readiness at a rule level.
--
-- 'rules', 'getReadinessCheckResourceStatusResponse_rules' - Details of the rule\'s results.
--
-- 'httpStatus', 'getReadinessCheckResourceStatusResponse_httpStatus' - The response's http status code.
newGetReadinessCheckResourceStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReadinessCheckResourceStatusResponse
newGetReadinessCheckResourceStatusResponse
  pHttpStatus_ =
    GetReadinessCheckResourceStatusResponse'
      { nextToken =
          Prelude.Nothing,
        readiness = Prelude.Nothing,
        rules = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token that identifies which batch of results you want to see.
getReadinessCheckResourceStatusResponse_nextToken :: Lens.Lens' GetReadinessCheckResourceStatusResponse (Prelude.Maybe Prelude.Text)
getReadinessCheckResourceStatusResponse_nextToken = Lens.lens (\GetReadinessCheckResourceStatusResponse' {nextToken} -> nextToken) (\s@GetReadinessCheckResourceStatusResponse' {} a -> s {nextToken = a} :: GetReadinessCheckResourceStatusResponse)

-- | The readiness at a rule level.
getReadinessCheckResourceStatusResponse_readiness :: Lens.Lens' GetReadinessCheckResourceStatusResponse (Prelude.Maybe Readiness)
getReadinessCheckResourceStatusResponse_readiness = Lens.lens (\GetReadinessCheckResourceStatusResponse' {readiness} -> readiness) (\s@GetReadinessCheckResourceStatusResponse' {} a -> s {readiness = a} :: GetReadinessCheckResourceStatusResponse)

-- | Details of the rule\'s results.
getReadinessCheckResourceStatusResponse_rules :: Lens.Lens' GetReadinessCheckResourceStatusResponse (Prelude.Maybe [RuleResult])
getReadinessCheckResourceStatusResponse_rules = Lens.lens (\GetReadinessCheckResourceStatusResponse' {rules} -> rules) (\s@GetReadinessCheckResourceStatusResponse' {} a -> s {rules = a} :: GetReadinessCheckResourceStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getReadinessCheckResourceStatusResponse_httpStatus :: Lens.Lens' GetReadinessCheckResourceStatusResponse Prelude.Int
getReadinessCheckResourceStatusResponse_httpStatus = Lens.lens (\GetReadinessCheckResourceStatusResponse' {httpStatus} -> httpStatus) (\s@GetReadinessCheckResourceStatusResponse' {} a -> s {httpStatus = a} :: GetReadinessCheckResourceStatusResponse)

instance
  Prelude.NFData
    GetReadinessCheckResourceStatusResponse
  where
  rnf GetReadinessCheckResourceStatusResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf readiness
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf httpStatus
