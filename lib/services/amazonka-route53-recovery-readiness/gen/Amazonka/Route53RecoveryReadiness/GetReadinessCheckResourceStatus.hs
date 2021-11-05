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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed information about the status of an individual resource
-- within a Readiness Check\'s Resource Set.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.GetReadinessCheckResourceStatus
  ( -- * Creating a Request
    GetReadinessCheckResourceStatus (..),
    newGetReadinessCheckResourceStatus,

    -- * Request Lenses
    getReadinessCheckResourceStatus_nextToken,
    getReadinessCheckResourceStatus_maxResults,
    getReadinessCheckResourceStatus_readinessCheckName,
    getReadinessCheckResourceStatus_resourceIdentifier,

    -- * Destructuring the Response
    GetReadinessCheckResourceStatusResponse (..),
    newGetReadinessCheckResourceStatusResponse,

    -- * Response Lenses
    getReadinessCheckResourceStatusResponse_rules,
    getReadinessCheckResourceStatusResponse_readiness,
    getReadinessCheckResourceStatusResponse_nextToken,
    getReadinessCheckResourceStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetReadinessCheckResourceStatus' smart constructor.
data GetReadinessCheckResourceStatus = GetReadinessCheckResourceStatus'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ReadinessCheck to get
    readinessCheckName :: Prelude.Text,
    -- | The resource ARN or component Id to get
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
-- 'nextToken', 'getReadinessCheckResourceStatus_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'getReadinessCheckResourceStatus_maxResults' - Upper bound on number of records to return.
--
-- 'readinessCheckName', 'getReadinessCheckResourceStatus_readinessCheckName' - The ReadinessCheck to get
--
-- 'resourceIdentifier', 'getReadinessCheckResourceStatus_resourceIdentifier' - The resource ARN or component Id to get
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
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        readinessCheckName = pReadinessCheckName_,
        resourceIdentifier = pResourceIdentifier_
      }

-- | A token used to resume pagination from the end of a previous request.
getReadinessCheckResourceStatus_nextToken :: Lens.Lens' GetReadinessCheckResourceStatus (Prelude.Maybe Prelude.Text)
getReadinessCheckResourceStatus_nextToken = Lens.lens (\GetReadinessCheckResourceStatus' {nextToken} -> nextToken) (\s@GetReadinessCheckResourceStatus' {} a -> s {nextToken = a} :: GetReadinessCheckResourceStatus)

-- | Upper bound on number of records to return.
getReadinessCheckResourceStatus_maxResults :: Lens.Lens' GetReadinessCheckResourceStatus (Prelude.Maybe Prelude.Natural)
getReadinessCheckResourceStatus_maxResults = Lens.lens (\GetReadinessCheckResourceStatus' {maxResults} -> maxResults) (\s@GetReadinessCheckResourceStatus' {} a -> s {maxResults = a} :: GetReadinessCheckResourceStatus)

-- | The ReadinessCheck to get
getReadinessCheckResourceStatus_readinessCheckName :: Lens.Lens' GetReadinessCheckResourceStatus Prelude.Text
getReadinessCheckResourceStatus_readinessCheckName = Lens.lens (\GetReadinessCheckResourceStatus' {readinessCheckName} -> readinessCheckName) (\s@GetReadinessCheckResourceStatus' {} a -> s {readinessCheckName = a} :: GetReadinessCheckResourceStatus)

-- | The resource ARN or component Id to get
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
      Prelude.Just Prelude.$
        rq
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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReadinessCheckResourceStatusResponse'
            Prelude.<$> (x Core..?> "rules" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "readiness")
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetReadinessCheckResourceStatus

instance
  Prelude.NFData
    GetReadinessCheckResourceStatus

instance
  Core.ToHeaders
    GetReadinessCheckResourceStatus
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

instance Core.ToPath GetReadinessCheckResourceStatus where
  toPath GetReadinessCheckResourceStatus' {..} =
    Prelude.mconcat
      [ "/readinesschecks/",
        Core.toBS readinessCheckName,
        "/resource/",
        Core.toBS resourceIdentifier,
        "/status"
      ]

instance Core.ToQuery GetReadinessCheckResourceStatus where
  toQuery GetReadinessCheckResourceStatus' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetReadinessCheckResourceStatusResponse' smart constructor.
data GetReadinessCheckResourceStatusResponse = GetReadinessCheckResourceStatusResponse'
  { -- | Details of the rules\'s results
    rules :: Prelude.Maybe [RuleResult],
    -- | The readiness at rule level.
    readiness :: Prelude.Maybe Readiness,
    -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'rules', 'getReadinessCheckResourceStatusResponse_rules' - Details of the rules\'s results
--
-- 'readiness', 'getReadinessCheckResourceStatusResponse_readiness' - The readiness at rule level.
--
-- 'nextToken', 'getReadinessCheckResourceStatusResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'httpStatus', 'getReadinessCheckResourceStatusResponse_httpStatus' - The response's http status code.
newGetReadinessCheckResourceStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReadinessCheckResourceStatusResponse
newGetReadinessCheckResourceStatusResponse
  pHttpStatus_ =
    GetReadinessCheckResourceStatusResponse'
      { rules =
          Prelude.Nothing,
        readiness = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Details of the rules\'s results
getReadinessCheckResourceStatusResponse_rules :: Lens.Lens' GetReadinessCheckResourceStatusResponse (Prelude.Maybe [RuleResult])
getReadinessCheckResourceStatusResponse_rules = Lens.lens (\GetReadinessCheckResourceStatusResponse' {rules} -> rules) (\s@GetReadinessCheckResourceStatusResponse' {} a -> s {rules = a} :: GetReadinessCheckResourceStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The readiness at rule level.
getReadinessCheckResourceStatusResponse_readiness :: Lens.Lens' GetReadinessCheckResourceStatusResponse (Prelude.Maybe Readiness)
getReadinessCheckResourceStatusResponse_readiness = Lens.lens (\GetReadinessCheckResourceStatusResponse' {readiness} -> readiness) (\s@GetReadinessCheckResourceStatusResponse' {} a -> s {readiness = a} :: GetReadinessCheckResourceStatusResponse)

-- | A token that can be used to resume pagination from the end of the
-- collection.
getReadinessCheckResourceStatusResponse_nextToken :: Lens.Lens' GetReadinessCheckResourceStatusResponse (Prelude.Maybe Prelude.Text)
getReadinessCheckResourceStatusResponse_nextToken = Lens.lens (\GetReadinessCheckResourceStatusResponse' {nextToken} -> nextToken) (\s@GetReadinessCheckResourceStatusResponse' {} a -> s {nextToken = a} :: GetReadinessCheckResourceStatusResponse)

-- | The response's http status code.
getReadinessCheckResourceStatusResponse_httpStatus :: Lens.Lens' GetReadinessCheckResourceStatusResponse Prelude.Int
getReadinessCheckResourceStatusResponse_httpStatus = Lens.lens (\GetReadinessCheckResourceStatusResponse' {httpStatus} -> httpStatus) (\s@GetReadinessCheckResourceStatusResponse' {} a -> s {httpStatus = a} :: GetReadinessCheckResourceStatusResponse)

instance
  Prelude.NFData
    GetReadinessCheckResourceStatusResponse
