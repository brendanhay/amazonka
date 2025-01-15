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
-- Module      : Amazonka.ResourceExplorer2.UpdateIndexType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the type of the index from one of the following types to the
-- other. For more information about indexes and the role they perform in
-- Amazon Web Services Resource Explorer, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/manage-aggregator-region.html Turning on cross-Region search by creating an aggregator index>
-- in the /Amazon Web Services Resource Explorer User Guide/.
--
-- -   __@AGGREGATOR@ index type__
--
--     The index contains information about resources from all Amazon Web
--     Services Regions in the Amazon Web Services account in which you\'ve
--     created a Resource Explorer index. Resource information from all
--     other Regions is replicated to this Region\'s index.
--
--     When you change the index type to @AGGREGATOR@, Resource Explorer
--     turns on replication of all discovered resource information from the
--     other Amazon Web Services Regions in your account to this index. You
--     can then, from this Region only, perform resource search queries
--     that span all Amazon Web Services Regions in the Amazon Web Services
--     account. Turning on replication from all other Regions is performed
--     by asynchronous background tasks. You can check the status of the
--     asynchronous tasks by using the GetIndex operation. When the
--     asynchronous tasks complete, the @Status@ response of that operation
--     changes from @UPDATING@ to @ACTIVE@. After that, you can start to
--     see results from other Amazon Web Services Regions in query results.
--     However, it can take several hours for replication from all other
--     Regions to complete.
--
--     You can have only one aggregator index per Amazon Web Services
--     account. Before you can promote a different index to be the
--     aggregator index for the account, you must first demote the existing
--     aggregator index to type @LOCAL@.
--
-- -   __@LOCAL@ index type__
--
--     The index contains information about resources in only the Amazon
--     Web Services Region in which the index exists. If an aggregator
--     index in another Region exists, then information in this local index
--     is replicated to the aggregator index.
--
--     When you change the index type to @LOCAL@, Resource Explorer turns
--     off the replication of resource information from all other Amazon
--     Web Services Regions in the Amazon Web Services account to this
--     Region. The aggregator index remains in the @UPDATING@ state until
--     all replication with other Regions successfully stops. You can check
--     the status of the asynchronous task by using the GetIndex operation.
--     When Resource Explorer successfully stops all replication with other
--     Regions, the @Status@ response of that operation changes from
--     @UPDATING@ to @ACTIVE@. Separately, the resource information from
--     other Regions that was previously stored in the index is deleted
--     within 30 days by another background task. Until that asynchronous
--     task completes, some results from other Regions can continue to
--     appear in search results.
--
--     After you demote an aggregator index to a local index, you must wait
--     24 hours before you can promote another index to be the new
--     aggregator index for the account.
module Amazonka.ResourceExplorer2.UpdateIndexType
  ( -- * Creating a Request
    UpdateIndexType (..),
    newUpdateIndexType,

    -- * Request Lenses
    updateIndexType_arn,
    updateIndexType_type,

    -- * Destructuring the Response
    UpdateIndexTypeResponse (..),
    newUpdateIndexTypeResponse,

    -- * Response Lenses
    updateIndexTypeResponse_arn,
    updateIndexTypeResponse_lastUpdatedAt,
    updateIndexTypeResponse_state,
    updateIndexTypeResponse_type,
    updateIndexTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateIndexType' smart constructor.
data UpdateIndexType = UpdateIndexType'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the index that you want to update.
    arn :: Prelude.Text,
    -- | The type of the index. To understand the difference between @LOCAL@ and
    -- @AGGREGATOR@, see
    -- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/manage-aggregator-region.html Turning on cross-Region search>
    -- in the /Amazon Web Services Resource Explorer User Guide/.
    type' :: IndexType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIndexType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateIndexType_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the index that you want to update.
--
-- 'type'', 'updateIndexType_type' - The type of the index. To understand the difference between @LOCAL@ and
-- @AGGREGATOR@, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/manage-aggregator-region.html Turning on cross-Region search>
-- in the /Amazon Web Services Resource Explorer User Guide/.
newUpdateIndexType ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'type''
  IndexType ->
  UpdateIndexType
newUpdateIndexType pArn_ pType_ =
  UpdateIndexType' {arn = pArn_, type' = pType_}

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the index that you want to update.
updateIndexType_arn :: Lens.Lens' UpdateIndexType Prelude.Text
updateIndexType_arn = Lens.lens (\UpdateIndexType' {arn} -> arn) (\s@UpdateIndexType' {} a -> s {arn = a} :: UpdateIndexType)

-- | The type of the index. To understand the difference between @LOCAL@ and
-- @AGGREGATOR@, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/manage-aggregator-region.html Turning on cross-Region search>
-- in the /Amazon Web Services Resource Explorer User Guide/.
updateIndexType_type :: Lens.Lens' UpdateIndexType IndexType
updateIndexType_type = Lens.lens (\UpdateIndexType' {type'} -> type') (\s@UpdateIndexType' {} a -> s {type' = a} :: UpdateIndexType)

instance Core.AWSRequest UpdateIndexType where
  type
    AWSResponse UpdateIndexType =
      UpdateIndexTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIndexTypeResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "LastUpdatedAt")
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIndexType where
  hashWithSalt _salt UpdateIndexType' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` type'

instance Prelude.NFData UpdateIndexType where
  rnf UpdateIndexType' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders UpdateIndexType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateIndexType where
  toJSON UpdateIndexType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Arn" Data..= arn),
            Prelude.Just ("Type" Data..= type')
          ]
      )

instance Data.ToPath UpdateIndexType where
  toPath = Prelude.const "/UpdateIndexType"

instance Data.ToQuery UpdateIndexType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIndexTypeResponse' smart constructor.
data UpdateIndexTypeResponse = UpdateIndexTypeResponse'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the index that you updated.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and timestamp when the index was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | Indicates the state of the request to update the index. This operation
    -- is asynchronous. Call the GetIndex operation to check for changes.
    state :: Prelude.Maybe IndexState,
    -- | Specifies the type of the specified index after the operation completes.
    type' :: Prelude.Maybe IndexType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIndexTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateIndexTypeResponse_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the index that you updated.
--
-- 'lastUpdatedAt', 'updateIndexTypeResponse_lastUpdatedAt' - The date and timestamp when the index was last updated.
--
-- 'state', 'updateIndexTypeResponse_state' - Indicates the state of the request to update the index. This operation
-- is asynchronous. Call the GetIndex operation to check for changes.
--
-- 'type'', 'updateIndexTypeResponse_type' - Specifies the type of the specified index after the operation completes.
--
-- 'httpStatus', 'updateIndexTypeResponse_httpStatus' - The response's http status code.
newUpdateIndexTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateIndexTypeResponse
newUpdateIndexTypeResponse pHttpStatus_ =
  UpdateIndexTypeResponse'
    { arn = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      state = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the index that you updated.
updateIndexTypeResponse_arn :: Lens.Lens' UpdateIndexTypeResponse (Prelude.Maybe Prelude.Text)
updateIndexTypeResponse_arn = Lens.lens (\UpdateIndexTypeResponse' {arn} -> arn) (\s@UpdateIndexTypeResponse' {} a -> s {arn = a} :: UpdateIndexTypeResponse)

-- | The date and timestamp when the index was last updated.
updateIndexTypeResponse_lastUpdatedAt :: Lens.Lens' UpdateIndexTypeResponse (Prelude.Maybe Prelude.UTCTime)
updateIndexTypeResponse_lastUpdatedAt = Lens.lens (\UpdateIndexTypeResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@UpdateIndexTypeResponse' {} a -> s {lastUpdatedAt = a} :: UpdateIndexTypeResponse) Prelude.. Lens.mapping Data._Time

-- | Indicates the state of the request to update the index. This operation
-- is asynchronous. Call the GetIndex operation to check for changes.
updateIndexTypeResponse_state :: Lens.Lens' UpdateIndexTypeResponse (Prelude.Maybe IndexState)
updateIndexTypeResponse_state = Lens.lens (\UpdateIndexTypeResponse' {state} -> state) (\s@UpdateIndexTypeResponse' {} a -> s {state = a} :: UpdateIndexTypeResponse)

-- | Specifies the type of the specified index after the operation completes.
updateIndexTypeResponse_type :: Lens.Lens' UpdateIndexTypeResponse (Prelude.Maybe IndexType)
updateIndexTypeResponse_type = Lens.lens (\UpdateIndexTypeResponse' {type'} -> type') (\s@UpdateIndexTypeResponse' {} a -> s {type' = a} :: UpdateIndexTypeResponse)

-- | The response's http status code.
updateIndexTypeResponse_httpStatus :: Lens.Lens' UpdateIndexTypeResponse Prelude.Int
updateIndexTypeResponse_httpStatus = Lens.lens (\UpdateIndexTypeResponse' {httpStatus} -> httpStatus) (\s@UpdateIndexTypeResponse' {} a -> s {httpStatus = a} :: UpdateIndexTypeResponse)

instance Prelude.NFData UpdateIndexTypeResponse where
  rnf UpdateIndexTypeResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf lastUpdatedAt `Prelude.seq`
        Prelude.rnf state `Prelude.seq`
          Prelude.rnf type' `Prelude.seq`
            Prelude.rnf httpStatus
