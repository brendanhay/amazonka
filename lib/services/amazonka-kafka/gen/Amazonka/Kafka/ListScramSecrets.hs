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
-- Module      : Amazonka.Kafka.ListScramSecrets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the Scram Secrets associated with an Amazon MSK
-- cluster.
--
-- This operation returns paginated results.
module Amazonka.Kafka.ListScramSecrets
  ( -- * Creating a Request
    ListScramSecrets (..),
    newListScramSecrets,

    -- * Request Lenses
    listScramSecrets_maxResults,
    listScramSecrets_nextToken,
    listScramSecrets_clusterArn,

    -- * Destructuring the Response
    ListScramSecretsResponse (..),
    newListScramSecretsResponse,

    -- * Response Lenses
    listScramSecretsResponse_nextToken,
    listScramSecretsResponse_secretArnList,
    listScramSecretsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListScramSecrets' smart constructor.
data ListScramSecrets = ListScramSecrets'
  { -- | The maxResults of the query.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The nextToken of the query.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The arn of the cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScramSecrets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listScramSecrets_maxResults' - The maxResults of the query.
--
-- 'nextToken', 'listScramSecrets_nextToken' - The nextToken of the query.
--
-- 'clusterArn', 'listScramSecrets_clusterArn' - The arn of the cluster.
newListScramSecrets ::
  -- | 'clusterArn'
  Prelude.Text ->
  ListScramSecrets
newListScramSecrets pClusterArn_ =
  ListScramSecrets'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      clusterArn = pClusterArn_
    }

-- | The maxResults of the query.
listScramSecrets_maxResults :: Lens.Lens' ListScramSecrets (Prelude.Maybe Prelude.Natural)
listScramSecrets_maxResults = Lens.lens (\ListScramSecrets' {maxResults} -> maxResults) (\s@ListScramSecrets' {} a -> s {maxResults = a} :: ListScramSecrets)

-- | The nextToken of the query.
listScramSecrets_nextToken :: Lens.Lens' ListScramSecrets (Prelude.Maybe Prelude.Text)
listScramSecrets_nextToken = Lens.lens (\ListScramSecrets' {nextToken} -> nextToken) (\s@ListScramSecrets' {} a -> s {nextToken = a} :: ListScramSecrets)

-- | The arn of the cluster.
listScramSecrets_clusterArn :: Lens.Lens' ListScramSecrets Prelude.Text
listScramSecrets_clusterArn = Lens.lens (\ListScramSecrets' {clusterArn} -> clusterArn) (\s@ListScramSecrets' {} a -> s {clusterArn = a} :: ListScramSecrets)

instance Core.AWSPager ListScramSecrets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listScramSecretsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listScramSecretsResponse_secretArnList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listScramSecrets_nextToken
          Lens..~ rs
          Lens.^? listScramSecretsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListScramSecrets where
  type
    AWSResponse ListScramSecrets =
      ListScramSecretsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListScramSecretsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "secretArnList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListScramSecrets where
  hashWithSalt _salt ListScramSecrets' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData ListScramSecrets where
  rnf ListScramSecrets' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf clusterArn

instance Data.ToHeaders ListScramSecrets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListScramSecrets where
  toPath ListScramSecrets' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Data.toBS clusterArn,
        "/scram-secrets"
      ]

instance Data.ToQuery ListScramSecrets where
  toQuery ListScramSecrets' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListScramSecretsResponse' smart constructor.
data ListScramSecretsResponse = ListScramSecretsResponse'
  { -- | Paginated results marker.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of scram secrets associated with the cluster.
    secretArnList :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListScramSecretsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listScramSecretsResponse_nextToken' - Paginated results marker.
--
-- 'secretArnList', 'listScramSecretsResponse_secretArnList' - The list of scram secrets associated with the cluster.
--
-- 'httpStatus', 'listScramSecretsResponse_httpStatus' - The response's http status code.
newListScramSecretsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListScramSecretsResponse
newListScramSecretsResponse pHttpStatus_ =
  ListScramSecretsResponse'
    { nextToken =
        Prelude.Nothing,
      secretArnList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Paginated results marker.
listScramSecretsResponse_nextToken :: Lens.Lens' ListScramSecretsResponse (Prelude.Maybe Prelude.Text)
listScramSecretsResponse_nextToken = Lens.lens (\ListScramSecretsResponse' {nextToken} -> nextToken) (\s@ListScramSecretsResponse' {} a -> s {nextToken = a} :: ListScramSecretsResponse)

-- | The list of scram secrets associated with the cluster.
listScramSecretsResponse_secretArnList :: Lens.Lens' ListScramSecretsResponse (Prelude.Maybe [Prelude.Text])
listScramSecretsResponse_secretArnList = Lens.lens (\ListScramSecretsResponse' {secretArnList} -> secretArnList) (\s@ListScramSecretsResponse' {} a -> s {secretArnList = a} :: ListScramSecretsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listScramSecretsResponse_httpStatus :: Lens.Lens' ListScramSecretsResponse Prelude.Int
listScramSecretsResponse_httpStatus = Lens.lens (\ListScramSecretsResponse' {httpStatus} -> httpStatus) (\s@ListScramSecretsResponse' {} a -> s {httpStatus = a} :: ListScramSecretsResponse)

instance Prelude.NFData ListScramSecretsResponse where
  rnf ListScramSecretsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf secretArnList
      `Prelude.seq` Prelude.rnf httpStatus
