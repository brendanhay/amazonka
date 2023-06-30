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
-- Module      : Amazonka.ResourceExplorer2.GetIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details about the Amazon Web Services Resource Explorer index
-- in the Amazon Web Services Region in which you invoked the operation.
module Amazonka.ResourceExplorer2.GetIndex
  ( -- * Creating a Request
    GetIndex (..),
    newGetIndex,

    -- * Destructuring the Response
    GetIndexResponse (..),
    newGetIndexResponse,

    -- * Response Lenses
    getIndexResponse_arn,
    getIndexResponse_createdAt,
    getIndexResponse_lastUpdatedAt,
    getIndexResponse_replicatingFrom,
    getIndexResponse_replicatingTo,
    getIndexResponse_state,
    getIndexResponse_tags,
    getIndexResponse_type,
    getIndexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import Amazonka.ResourceExplorer2.Types
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIndex' smart constructor.
data GetIndex = GetIndex'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetIndex ::
  GetIndex
newGetIndex = GetIndex'

instance Core.AWSRequest GetIndex where
  type AWSResponse GetIndex = GetIndexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIndexResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreatedAt")
            Prelude.<*> (x Data..?> "LastUpdatedAt")
            Prelude.<*> ( x
                            Data..?> "ReplicatingFrom"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ReplicatingTo" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "State")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Type")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIndex where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetIndex where
  rnf _ = ()

instance Data.ToHeaders GetIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetIndex where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath GetIndex where
  toPath = Prelude.const "/GetIndex"

instance Data.ToQuery GetIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIndexResponse' smart constructor.
data GetIndexResponse = GetIndexResponse'
  { -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
    -- of the index.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the index was originally created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The date and time when the index was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.ISO8601,
    -- | This response value is present only if this index is @Type=AGGREGATOR@.
    --
    -- A list of the Amazon Web Services Regions that replicate their content
    -- to the index in this Region.
    replicatingFrom :: Prelude.Maybe [Prelude.Text],
    -- | This response value is present only if this index is @Type=LOCAL@.
    --
    -- The Amazon Web Services Region that contains the aggregator index, if
    -- one exists. If an aggregator index does exist then the Region in which
    -- you called this operation replicates its index information to the Region
    -- specified in this response value.
    replicatingTo :: Prelude.Maybe [Prelude.Text],
    -- | The current state of the index in this Amazon Web Services Region.
    state :: Prelude.Maybe IndexState,
    -- | Tag key and value pairs that are attached to the index.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The type of the index in this Region. For information about the
    -- aggregator index and how it differs from a local index, see
    -- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/manage-aggregator-region.html Turning on cross-Region search by creating an aggregator index>.
    type' :: Prelude.Maybe IndexType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getIndexResponse_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the index.
--
-- 'createdAt', 'getIndexResponse_createdAt' - The date and time when the index was originally created.
--
-- 'lastUpdatedAt', 'getIndexResponse_lastUpdatedAt' - The date and time when the index was last updated.
--
-- 'replicatingFrom', 'getIndexResponse_replicatingFrom' - This response value is present only if this index is @Type=AGGREGATOR@.
--
-- A list of the Amazon Web Services Regions that replicate their content
-- to the index in this Region.
--
-- 'replicatingTo', 'getIndexResponse_replicatingTo' - This response value is present only if this index is @Type=LOCAL@.
--
-- The Amazon Web Services Region that contains the aggregator index, if
-- one exists. If an aggregator index does exist then the Region in which
-- you called this operation replicates its index information to the Region
-- specified in this response value.
--
-- 'state', 'getIndexResponse_state' - The current state of the index in this Amazon Web Services Region.
--
-- 'tags', 'getIndexResponse_tags' - Tag key and value pairs that are attached to the index.
--
-- 'type'', 'getIndexResponse_type' - The type of the index in this Region. For information about the
-- aggregator index and how it differs from a local index, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/manage-aggregator-region.html Turning on cross-Region search by creating an aggregator index>.
--
-- 'httpStatus', 'getIndexResponse_httpStatus' - The response's http status code.
newGetIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIndexResponse
newGetIndexResponse pHttpStatus_ =
  GetIndexResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      replicatingFrom = Prelude.Nothing,
      replicatingTo = Prelude.Nothing,
      state = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon resource name (ARN)>
-- of the index.
getIndexResponse_arn :: Lens.Lens' GetIndexResponse (Prelude.Maybe Prelude.Text)
getIndexResponse_arn = Lens.lens (\GetIndexResponse' {arn} -> arn) (\s@GetIndexResponse' {} a -> s {arn = a} :: GetIndexResponse)

-- | The date and time when the index was originally created.
getIndexResponse_createdAt :: Lens.Lens' GetIndexResponse (Prelude.Maybe Prelude.UTCTime)
getIndexResponse_createdAt = Lens.lens (\GetIndexResponse' {createdAt} -> createdAt) (\s@GetIndexResponse' {} a -> s {createdAt = a} :: GetIndexResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time when the index was last updated.
getIndexResponse_lastUpdatedAt :: Lens.Lens' GetIndexResponse (Prelude.Maybe Prelude.UTCTime)
getIndexResponse_lastUpdatedAt = Lens.lens (\GetIndexResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@GetIndexResponse' {} a -> s {lastUpdatedAt = a} :: GetIndexResponse) Prelude.. Lens.mapping Data._Time

-- | This response value is present only if this index is @Type=AGGREGATOR@.
--
-- A list of the Amazon Web Services Regions that replicate their content
-- to the index in this Region.
getIndexResponse_replicatingFrom :: Lens.Lens' GetIndexResponse (Prelude.Maybe [Prelude.Text])
getIndexResponse_replicatingFrom = Lens.lens (\GetIndexResponse' {replicatingFrom} -> replicatingFrom) (\s@GetIndexResponse' {} a -> s {replicatingFrom = a} :: GetIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | This response value is present only if this index is @Type=LOCAL@.
--
-- The Amazon Web Services Region that contains the aggregator index, if
-- one exists. If an aggregator index does exist then the Region in which
-- you called this operation replicates its index information to the Region
-- specified in this response value.
getIndexResponse_replicatingTo :: Lens.Lens' GetIndexResponse (Prelude.Maybe [Prelude.Text])
getIndexResponse_replicatingTo = Lens.lens (\GetIndexResponse' {replicatingTo} -> replicatingTo) (\s@GetIndexResponse' {} a -> s {replicatingTo = a} :: GetIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The current state of the index in this Amazon Web Services Region.
getIndexResponse_state :: Lens.Lens' GetIndexResponse (Prelude.Maybe IndexState)
getIndexResponse_state = Lens.lens (\GetIndexResponse' {state} -> state) (\s@GetIndexResponse' {} a -> s {state = a} :: GetIndexResponse)

-- | Tag key and value pairs that are attached to the index.
getIndexResponse_tags :: Lens.Lens' GetIndexResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getIndexResponse_tags = Lens.lens (\GetIndexResponse' {tags} -> tags) (\s@GetIndexResponse' {} a -> s {tags = a} :: GetIndexResponse) Prelude.. Lens.mapping Lens.coerced

-- | The type of the index in this Region. For information about the
-- aggregator index and how it differs from a local index, see
-- <https://docs.aws.amazon.com/resource-explorer/latest/userguide/manage-aggregator-region.html Turning on cross-Region search by creating an aggregator index>.
getIndexResponse_type :: Lens.Lens' GetIndexResponse (Prelude.Maybe IndexType)
getIndexResponse_type = Lens.lens (\GetIndexResponse' {type'} -> type') (\s@GetIndexResponse' {} a -> s {type' = a} :: GetIndexResponse)

-- | The response's http status code.
getIndexResponse_httpStatus :: Lens.Lens' GetIndexResponse Prelude.Int
getIndexResponse_httpStatus = Lens.lens (\GetIndexResponse' {httpStatus} -> httpStatus) (\s@GetIndexResponse' {} a -> s {httpStatus = a} :: GetIndexResponse)

instance Prelude.NFData GetIndexResponse where
  rnf GetIndexResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf replicatingFrom
      `Prelude.seq` Prelude.rnf replicatingTo
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf httpStatus
