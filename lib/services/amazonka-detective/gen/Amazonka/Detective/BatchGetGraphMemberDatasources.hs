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
-- Module      : Amazonka.Detective.BatchGetGraphMemberDatasources
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets data source package information for the behavior graph.
module Amazonka.Detective.BatchGetGraphMemberDatasources
  ( -- * Creating a Request
    BatchGetGraphMemberDatasources (..),
    newBatchGetGraphMemberDatasources,

    -- * Request Lenses
    batchGetGraphMemberDatasources_graphArn,
    batchGetGraphMemberDatasources_accountIds,

    -- * Destructuring the Response
    BatchGetGraphMemberDatasourcesResponse (..),
    newBatchGetGraphMemberDatasourcesResponse,

    -- * Response Lenses
    batchGetGraphMemberDatasourcesResponse_unprocessedAccounts,
    batchGetGraphMemberDatasourcesResponse_memberDatasources,
    batchGetGraphMemberDatasourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Detective.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetGraphMemberDatasources' smart constructor.
data BatchGetGraphMemberDatasources = BatchGetGraphMemberDatasources'
  { -- | The ARN of the behavior graph.
    graphArn :: Prelude.Text,
    -- | The list of Amazon Web Services accounts to get data source package
    -- information on.
    accountIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetGraphMemberDatasources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'graphArn', 'batchGetGraphMemberDatasources_graphArn' - The ARN of the behavior graph.
--
-- 'accountIds', 'batchGetGraphMemberDatasources_accountIds' - The list of Amazon Web Services accounts to get data source package
-- information on.
newBatchGetGraphMemberDatasources ::
  -- | 'graphArn'
  Prelude.Text ->
  -- | 'accountIds'
  Prelude.NonEmpty Prelude.Text ->
  BatchGetGraphMemberDatasources
newBatchGetGraphMemberDatasources
  pGraphArn_
  pAccountIds_ =
    BatchGetGraphMemberDatasources'
      { graphArn =
          pGraphArn_,
        accountIds =
          Lens.coerced Lens.# pAccountIds_
      }

-- | The ARN of the behavior graph.
batchGetGraphMemberDatasources_graphArn :: Lens.Lens' BatchGetGraphMemberDatasources Prelude.Text
batchGetGraphMemberDatasources_graphArn = Lens.lens (\BatchGetGraphMemberDatasources' {graphArn} -> graphArn) (\s@BatchGetGraphMemberDatasources' {} a -> s {graphArn = a} :: BatchGetGraphMemberDatasources)

-- | The list of Amazon Web Services accounts to get data source package
-- information on.
batchGetGraphMemberDatasources_accountIds :: Lens.Lens' BatchGetGraphMemberDatasources (Prelude.NonEmpty Prelude.Text)
batchGetGraphMemberDatasources_accountIds = Lens.lens (\BatchGetGraphMemberDatasources' {accountIds} -> accountIds) (\s@BatchGetGraphMemberDatasources' {} a -> s {accountIds = a} :: BatchGetGraphMemberDatasources) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    BatchGetGraphMemberDatasources
  where
  type
    AWSResponse BatchGetGraphMemberDatasources =
      BatchGetGraphMemberDatasourcesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetGraphMemberDatasourcesResponse'
            Prelude.<$> ( x Data..?> "UnprocessedAccounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x Data..?> "MemberDatasources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchGetGraphMemberDatasources
  where
  hashWithSalt
    _salt
    BatchGetGraphMemberDatasources' {..} =
      _salt `Prelude.hashWithSalt` graphArn
        `Prelude.hashWithSalt` accountIds

instance
  Prelude.NFData
    BatchGetGraphMemberDatasources
  where
  rnf BatchGetGraphMemberDatasources' {..} =
    Prelude.rnf graphArn
      `Prelude.seq` Prelude.rnf accountIds

instance
  Data.ToHeaders
    BatchGetGraphMemberDatasources
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

instance Data.ToJSON BatchGetGraphMemberDatasources where
  toJSON BatchGetGraphMemberDatasources' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GraphArn" Data..= graphArn),
            Prelude.Just ("AccountIds" Data..= accountIds)
          ]
      )

instance Data.ToPath BatchGetGraphMemberDatasources where
  toPath = Prelude.const "/graph/datasources/get"

instance Data.ToQuery BatchGetGraphMemberDatasources where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetGraphMemberDatasourcesResponse' smart constructor.
data BatchGetGraphMemberDatasourcesResponse = BatchGetGraphMemberDatasourcesResponse'
  { -- | Accounts that data source package information could not be retrieved
    -- for.
    unprocessedAccounts :: Prelude.Maybe [UnprocessedAccount],
    -- | Details on the status of data source packages for members of the
    -- behavior graph.
    memberDatasources :: Prelude.Maybe [MembershipDatasources],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetGraphMemberDatasourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedAccounts', 'batchGetGraphMemberDatasourcesResponse_unprocessedAccounts' - Accounts that data source package information could not be retrieved
-- for.
--
-- 'memberDatasources', 'batchGetGraphMemberDatasourcesResponse_memberDatasources' - Details on the status of data source packages for members of the
-- behavior graph.
--
-- 'httpStatus', 'batchGetGraphMemberDatasourcesResponse_httpStatus' - The response's http status code.
newBatchGetGraphMemberDatasourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetGraphMemberDatasourcesResponse
newBatchGetGraphMemberDatasourcesResponse
  pHttpStatus_ =
    BatchGetGraphMemberDatasourcesResponse'
      { unprocessedAccounts =
          Prelude.Nothing,
        memberDatasources = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Accounts that data source package information could not be retrieved
-- for.
batchGetGraphMemberDatasourcesResponse_unprocessedAccounts :: Lens.Lens' BatchGetGraphMemberDatasourcesResponse (Prelude.Maybe [UnprocessedAccount])
batchGetGraphMemberDatasourcesResponse_unprocessedAccounts = Lens.lens (\BatchGetGraphMemberDatasourcesResponse' {unprocessedAccounts} -> unprocessedAccounts) (\s@BatchGetGraphMemberDatasourcesResponse' {} a -> s {unprocessedAccounts = a} :: BatchGetGraphMemberDatasourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Details on the status of data source packages for members of the
-- behavior graph.
batchGetGraphMemberDatasourcesResponse_memberDatasources :: Lens.Lens' BatchGetGraphMemberDatasourcesResponse (Prelude.Maybe [MembershipDatasources])
batchGetGraphMemberDatasourcesResponse_memberDatasources = Lens.lens (\BatchGetGraphMemberDatasourcesResponse' {memberDatasources} -> memberDatasources) (\s@BatchGetGraphMemberDatasourcesResponse' {} a -> s {memberDatasources = a} :: BatchGetGraphMemberDatasourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetGraphMemberDatasourcesResponse_httpStatus :: Lens.Lens' BatchGetGraphMemberDatasourcesResponse Prelude.Int
batchGetGraphMemberDatasourcesResponse_httpStatus = Lens.lens (\BatchGetGraphMemberDatasourcesResponse' {httpStatus} -> httpStatus) (\s@BatchGetGraphMemberDatasourcesResponse' {} a -> s {httpStatus = a} :: BatchGetGraphMemberDatasourcesResponse)

instance
  Prelude.NFData
    BatchGetGraphMemberDatasourcesResponse
  where
  rnf BatchGetGraphMemberDatasourcesResponse' {..} =
    Prelude.rnf unprocessedAccounts
      `Prelude.seq` Prelude.rnf memberDatasources
      `Prelude.seq` Prelude.rnf httpStatus
