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
-- Module      : Amazonka.Detective.BatchGetMembershipDatasources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information on the data source package history for an account.
module Amazonka.Detective.BatchGetMembershipDatasources
  ( -- * Creating a Request
    BatchGetMembershipDatasources (..),
    newBatchGetMembershipDatasources,

    -- * Request Lenses
    batchGetMembershipDatasources_graphArns,

    -- * Destructuring the Response
    BatchGetMembershipDatasourcesResponse (..),
    newBatchGetMembershipDatasourcesResponse,

    -- * Response Lenses
    batchGetMembershipDatasourcesResponse_membershipDatasources,
    batchGetMembershipDatasourcesResponse_unprocessedGraphs,
    batchGetMembershipDatasourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetMembershipDatasources' smart constructor.
data BatchGetMembershipDatasources = BatchGetMembershipDatasources'
  { -- | The ARN of the behavior graph.
    graphArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetMembershipDatasources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphArns', 'batchGetMembershipDatasources_graphArns' - The ARN of the behavior graph.
newBatchGetMembershipDatasources ::
  -- | 'graphArns'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetMembershipDatasources
newBatchGetMembershipDatasources pGraphArns_ =
  BatchGetMembershipDatasources'
    { graphArns =
        Lens.coerced Lens.# pGraphArns_
    }

-- | The ARN of the behavior graph.
batchGetMembershipDatasources_graphArns :: Lens.Lens' BatchGetMembershipDatasources (Prelude.NonEmpty Prelude.Text)
batchGetMembershipDatasources_graphArns = Lens.lens (\BatchGetMembershipDatasources' {graphArns} -> graphArns) (\s@BatchGetMembershipDatasources' {} a -> s {graphArns = a} :: BatchGetMembershipDatasources) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchGetMembershipDatasources
  where
  type
    AWSResponse BatchGetMembershipDatasources =
      BatchGetMembershipDatasourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetMembershipDatasourcesResponse'
            Prelude.<$> ( x
                            Data..?> "MembershipDatasources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "UnprocessedGraphs"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchGetMembershipDatasources
  where
  hashWithSalt _salt BatchGetMembershipDatasources' {..} =
    _salt `Prelude.hashWithSalt` graphArns

instance Prelude.NFData BatchGetMembershipDatasources where
  rnf BatchGetMembershipDatasources' {..} =
    Prelude.rnf graphArns

instance Data.ToHeaders BatchGetMembershipDatasources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetMembershipDatasources where
  toJSON BatchGetMembershipDatasources' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GraphArns" Data..= graphArns)]
      )

instance Data.ToPath BatchGetMembershipDatasources where
  toPath = Prelude.const "/membership/datasources/get"

instance Data.ToQuery BatchGetMembershipDatasources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetMembershipDatasourcesResponse' smart constructor.
data BatchGetMembershipDatasourcesResponse = BatchGetMembershipDatasourcesResponse'
  { -- | Details on the data source package history for an member of the behavior
    -- graph.
    membershipDatasources :: Prelude.Maybe [MembershipDatasources],
    -- | Graphs that data source package information could not be retrieved for.
    unprocessedGraphs :: Prelude.Maybe [UnprocessedGraph],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetMembershipDatasourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'membershipDatasources', 'batchGetMembershipDatasourcesResponse_membershipDatasources' - Details on the data source package history for an member of the behavior
-- graph.
--
-- 'unprocessedGraphs', 'batchGetMembershipDatasourcesResponse_unprocessedGraphs' - Graphs that data source package information could not be retrieved for.
--
-- 'httpStatus', 'batchGetMembershipDatasourcesResponse_httpStatus' - The response's http status code.
newBatchGetMembershipDatasourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetMembershipDatasourcesResponse
newBatchGetMembershipDatasourcesResponse pHttpStatus_ =
  BatchGetMembershipDatasourcesResponse'
    { membershipDatasources =
        Prelude.Nothing,
      unprocessedGraphs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details on the data source package history for an member of the behavior
-- graph.
batchGetMembershipDatasourcesResponse_membershipDatasources :: Lens.Lens' BatchGetMembershipDatasourcesResponse (Prelude.Maybe [MembershipDatasources])
batchGetMembershipDatasourcesResponse_membershipDatasources = Lens.lens (\BatchGetMembershipDatasourcesResponse' {membershipDatasources} -> membershipDatasources) (\s@BatchGetMembershipDatasourcesResponse' {} a -> s {membershipDatasources = a} :: BatchGetMembershipDatasourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Graphs that data source package information could not be retrieved for.
batchGetMembershipDatasourcesResponse_unprocessedGraphs :: Lens.Lens' BatchGetMembershipDatasourcesResponse (Prelude.Maybe [UnprocessedGraph])
batchGetMembershipDatasourcesResponse_unprocessedGraphs = Lens.lens (\BatchGetMembershipDatasourcesResponse' {unprocessedGraphs} -> unprocessedGraphs) (\s@BatchGetMembershipDatasourcesResponse' {} a -> s {unprocessedGraphs = a} :: BatchGetMembershipDatasourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetMembershipDatasourcesResponse_httpStatus :: Lens.Lens' BatchGetMembershipDatasourcesResponse Prelude.Int
batchGetMembershipDatasourcesResponse_httpStatus = Lens.lens (\BatchGetMembershipDatasourcesResponse' {httpStatus} -> httpStatus) (\s@BatchGetMembershipDatasourcesResponse' {} a -> s {httpStatus = a} :: BatchGetMembershipDatasourcesResponse)

instance
  Prelude.NFData
    BatchGetMembershipDatasourcesResponse
  where
  rnf BatchGetMembershipDatasourcesResponse' {..} =
    Prelude.rnf membershipDatasources
      `Prelude.seq` Prelude.rnf unprocessedGraphs
      `Prelude.seq` Prelude.rnf httpStatus
