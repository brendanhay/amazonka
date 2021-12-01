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
-- Module      : Amazonka.Route53RecoveryReadiness.GetReadinessCheckStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the status of a Readiness Check.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryReadiness.GetReadinessCheckStatus
  ( -- * Creating a Request
    GetReadinessCheckStatus (..),
    newGetReadinessCheckStatus,

    -- * Request Lenses
    getReadinessCheckStatus_nextToken,
    getReadinessCheckStatus_maxResults,
    getReadinessCheckStatus_readinessCheckName,

    -- * Destructuring the Response
    GetReadinessCheckStatusResponse (..),
    newGetReadinessCheckStatusResponse,

    -- * Response Lenses
    getReadinessCheckStatusResponse_readiness,
    getReadinessCheckStatusResponse_resources,
    getReadinessCheckStatusResponse_nextToken,
    getReadinessCheckStatusResponse_messages,
    getReadinessCheckStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryReadiness.Types

-- | /See:/ 'newGetReadinessCheckStatus' smart constructor.
data GetReadinessCheckStatus = GetReadinessCheckStatus'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Upper bound on number of records to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ReadinessCheck to get
    readinessCheckName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadinessCheckStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getReadinessCheckStatus_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'maxResults', 'getReadinessCheckStatus_maxResults' - Upper bound on number of records to return.
--
-- 'readinessCheckName', 'getReadinessCheckStatus_readinessCheckName' - The ReadinessCheck to get
newGetReadinessCheckStatus ::
  -- | 'readinessCheckName'
  Prelude.Text ->
  GetReadinessCheckStatus
newGetReadinessCheckStatus pReadinessCheckName_ =
  GetReadinessCheckStatus'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      readinessCheckName = pReadinessCheckName_
    }

-- | A token used to resume pagination from the end of a previous request.
getReadinessCheckStatus_nextToken :: Lens.Lens' GetReadinessCheckStatus (Prelude.Maybe Prelude.Text)
getReadinessCheckStatus_nextToken = Lens.lens (\GetReadinessCheckStatus' {nextToken} -> nextToken) (\s@GetReadinessCheckStatus' {} a -> s {nextToken = a} :: GetReadinessCheckStatus)

-- | Upper bound on number of records to return.
getReadinessCheckStatus_maxResults :: Lens.Lens' GetReadinessCheckStatus (Prelude.Maybe Prelude.Natural)
getReadinessCheckStatus_maxResults = Lens.lens (\GetReadinessCheckStatus' {maxResults} -> maxResults) (\s@GetReadinessCheckStatus' {} a -> s {maxResults = a} :: GetReadinessCheckStatus)

-- | The ReadinessCheck to get
getReadinessCheckStatus_readinessCheckName :: Lens.Lens' GetReadinessCheckStatus Prelude.Text
getReadinessCheckStatus_readinessCheckName = Lens.lens (\GetReadinessCheckStatus' {readinessCheckName} -> readinessCheckName) (\s@GetReadinessCheckStatus' {} a -> s {readinessCheckName = a} :: GetReadinessCheckStatus)

instance Core.AWSPager GetReadinessCheckStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getReadinessCheckStatusResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getReadinessCheckStatusResponse_resources
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getReadinessCheckStatus_nextToken
          Lens..~ rs
          Lens.^? getReadinessCheckStatusResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetReadinessCheckStatus where
  type
    AWSResponse GetReadinessCheckStatus =
      GetReadinessCheckStatusResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetReadinessCheckStatusResponse'
            Prelude.<$> (x Core..?> "readiness")
            Prelude.<*> (x Core..?> "resources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "messages" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetReadinessCheckStatus where
  hashWithSalt salt' GetReadinessCheckStatus' {..} =
    salt' `Prelude.hashWithSalt` readinessCheckName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetReadinessCheckStatus where
  rnf GetReadinessCheckStatus' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf readinessCheckName
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders GetReadinessCheckStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetReadinessCheckStatus where
  toPath GetReadinessCheckStatus' {..} =
    Prelude.mconcat
      [ "/readinesschecks/",
        Core.toBS readinessCheckName,
        "/status"
      ]

instance Core.ToQuery GetReadinessCheckStatus where
  toQuery GetReadinessCheckStatus' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetReadinessCheckStatusResponse' smart constructor.
data GetReadinessCheckStatusResponse = GetReadinessCheckStatusResponse'
  { -- | The readiness at rule level.
    readiness :: Prelude.Maybe Readiness,
    -- | Summary of resources\'s readiness
    resources :: Prelude.Maybe [ResourceResult],
    -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Top level messages for readiness check status
    messages :: Prelude.Maybe [Message],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReadinessCheckStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readiness', 'getReadinessCheckStatusResponse_readiness' - The readiness at rule level.
--
-- 'resources', 'getReadinessCheckStatusResponse_resources' - Summary of resources\'s readiness
--
-- 'nextToken', 'getReadinessCheckStatusResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'messages', 'getReadinessCheckStatusResponse_messages' - Top level messages for readiness check status
--
-- 'httpStatus', 'getReadinessCheckStatusResponse_httpStatus' - The response's http status code.
newGetReadinessCheckStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetReadinessCheckStatusResponse
newGetReadinessCheckStatusResponse pHttpStatus_ =
  GetReadinessCheckStatusResponse'
    { readiness =
        Prelude.Nothing,
      resources = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      messages = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The readiness at rule level.
getReadinessCheckStatusResponse_readiness :: Lens.Lens' GetReadinessCheckStatusResponse (Prelude.Maybe Readiness)
getReadinessCheckStatusResponse_readiness = Lens.lens (\GetReadinessCheckStatusResponse' {readiness} -> readiness) (\s@GetReadinessCheckStatusResponse' {} a -> s {readiness = a} :: GetReadinessCheckStatusResponse)

-- | Summary of resources\'s readiness
getReadinessCheckStatusResponse_resources :: Lens.Lens' GetReadinessCheckStatusResponse (Prelude.Maybe [ResourceResult])
getReadinessCheckStatusResponse_resources = Lens.lens (\GetReadinessCheckStatusResponse' {resources} -> resources) (\s@GetReadinessCheckStatusResponse' {} a -> s {resources = a} :: GetReadinessCheckStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token that can be used to resume pagination from the end of the
-- collection.
getReadinessCheckStatusResponse_nextToken :: Lens.Lens' GetReadinessCheckStatusResponse (Prelude.Maybe Prelude.Text)
getReadinessCheckStatusResponse_nextToken = Lens.lens (\GetReadinessCheckStatusResponse' {nextToken} -> nextToken) (\s@GetReadinessCheckStatusResponse' {} a -> s {nextToken = a} :: GetReadinessCheckStatusResponse)

-- | Top level messages for readiness check status
getReadinessCheckStatusResponse_messages :: Lens.Lens' GetReadinessCheckStatusResponse (Prelude.Maybe [Message])
getReadinessCheckStatusResponse_messages = Lens.lens (\GetReadinessCheckStatusResponse' {messages} -> messages) (\s@GetReadinessCheckStatusResponse' {} a -> s {messages = a} :: GetReadinessCheckStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getReadinessCheckStatusResponse_httpStatus :: Lens.Lens' GetReadinessCheckStatusResponse Prelude.Int
getReadinessCheckStatusResponse_httpStatus = Lens.lens (\GetReadinessCheckStatusResponse' {httpStatus} -> httpStatus) (\s@GetReadinessCheckStatusResponse' {} a -> s {httpStatus = a} :: GetReadinessCheckStatusResponse)

instance
  Prelude.NFData
    GetReadinessCheckStatusResponse
  where
  rnf GetReadinessCheckStatusResponse' {..} =
    Prelude.rnf readiness
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf messages
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf resources
