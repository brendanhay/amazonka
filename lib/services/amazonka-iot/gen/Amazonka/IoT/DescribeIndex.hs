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
-- Module      : Amazonka.IoT.DescribeIndex
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a search index.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeIndex>
-- action.
module Amazonka.IoT.DescribeIndex
  ( -- * Creating a Request
    DescribeIndex (..),
    newDescribeIndex,

    -- * Request Lenses
    describeIndex_indexName,

    -- * Destructuring the Response
    DescribeIndexResponse (..),
    newDescribeIndexResponse,

    -- * Response Lenses
    describeIndexResponse_indexName,
    describeIndexResponse_indexStatus,
    describeIndexResponse_schema,
    describeIndexResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeIndex' smart constructor.
data DescribeIndex = DescribeIndex'
  { -- | The index name.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'describeIndex_indexName' - The index name.
newDescribeIndex ::
  -- | 'indexName'
  Prelude.Text ->
  DescribeIndex
newDescribeIndex pIndexName_ =
  DescribeIndex' {indexName = pIndexName_}

-- | The index name.
describeIndex_indexName :: Lens.Lens' DescribeIndex Prelude.Text
describeIndex_indexName = Lens.lens (\DescribeIndex' {indexName} -> indexName) (\s@DescribeIndex' {} a -> s {indexName = a} :: DescribeIndex)

instance Core.AWSRequest DescribeIndex where
  type
    AWSResponse DescribeIndex =
      DescribeIndexResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIndexResponse'
            Prelude.<$> (x Data..?> "indexName")
            Prelude.<*> (x Data..?> "indexStatus")
            Prelude.<*> (x Data..?> "schema")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIndex where
  hashWithSalt _salt DescribeIndex' {..} =
    _salt `Prelude.hashWithSalt` indexName

instance Prelude.NFData DescribeIndex where
  rnf DescribeIndex' {..} = Prelude.rnf indexName

instance Data.ToHeaders DescribeIndex where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeIndex where
  toPath DescribeIndex' {..} =
    Prelude.mconcat ["/indices/", Data.toBS indexName]

instance Data.ToQuery DescribeIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIndexResponse' smart constructor.
data DescribeIndexResponse = DescribeIndexResponse'
  { -- | The index name.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The index status.
    indexStatus :: Prelude.Maybe IndexStatus,
    -- | Contains a value that specifies the type of indexing performed. Valid
    -- values are:
    --
    -- -   REGISTRY – Your thing index contains only registry data.
    --
    -- -   REGISTRY_AND_SHADOW - Your thing index contains registry data and
    --     shadow data.
    --
    -- -   REGISTRY_AND_CONNECTIVITY_STATUS - Your thing index contains
    --     registry data and thing connectivity status data.
    --
    -- -   REGISTRY_AND_SHADOW_AND_CONNECTIVITY_STATUS - Your thing index
    --     contains registry data, shadow data, and thing connectivity status
    --     data.
    --
    -- -   MULTI_INDEXING_MODE - Your thing index contains multiple data
    --     sources. For more information, see
    --     <https://docs.aws.amazon.com/iot/latest/apireference/API_GetIndexingConfiguration.html GetIndexingConfiguration>.
    schema :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'indexName', 'describeIndexResponse_indexName' - The index name.
--
-- 'indexStatus', 'describeIndexResponse_indexStatus' - The index status.
--
-- 'schema', 'describeIndexResponse_schema' - Contains a value that specifies the type of indexing performed. Valid
-- values are:
--
-- -   REGISTRY – Your thing index contains only registry data.
--
-- -   REGISTRY_AND_SHADOW - Your thing index contains registry data and
--     shadow data.
--
-- -   REGISTRY_AND_CONNECTIVITY_STATUS - Your thing index contains
--     registry data and thing connectivity status data.
--
-- -   REGISTRY_AND_SHADOW_AND_CONNECTIVITY_STATUS - Your thing index
--     contains registry data, shadow data, and thing connectivity status
--     data.
--
-- -   MULTI_INDEXING_MODE - Your thing index contains multiple data
--     sources. For more information, see
--     <https://docs.aws.amazon.com/iot/latest/apireference/API_GetIndexingConfiguration.html GetIndexingConfiguration>.
--
-- 'httpStatus', 'describeIndexResponse_httpStatus' - The response's http status code.
newDescribeIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIndexResponse
newDescribeIndexResponse pHttpStatus_ =
  DescribeIndexResponse'
    { indexName = Prelude.Nothing,
      indexStatus = Prelude.Nothing,
      schema = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The index name.
describeIndexResponse_indexName :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_indexName = Lens.lens (\DescribeIndexResponse' {indexName} -> indexName) (\s@DescribeIndexResponse' {} a -> s {indexName = a} :: DescribeIndexResponse)

-- | The index status.
describeIndexResponse_indexStatus :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexStatus)
describeIndexResponse_indexStatus = Lens.lens (\DescribeIndexResponse' {indexStatus} -> indexStatus) (\s@DescribeIndexResponse' {} a -> s {indexStatus = a} :: DescribeIndexResponse)

-- | Contains a value that specifies the type of indexing performed. Valid
-- values are:
--
-- -   REGISTRY – Your thing index contains only registry data.
--
-- -   REGISTRY_AND_SHADOW - Your thing index contains registry data and
--     shadow data.
--
-- -   REGISTRY_AND_CONNECTIVITY_STATUS - Your thing index contains
--     registry data and thing connectivity status data.
--
-- -   REGISTRY_AND_SHADOW_AND_CONNECTIVITY_STATUS - Your thing index
--     contains registry data, shadow data, and thing connectivity status
--     data.
--
-- -   MULTI_INDEXING_MODE - Your thing index contains multiple data
--     sources. For more information, see
--     <https://docs.aws.amazon.com/iot/latest/apireference/API_GetIndexingConfiguration.html GetIndexingConfiguration>.
describeIndexResponse_schema :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_schema = Lens.lens (\DescribeIndexResponse' {schema} -> schema) (\s@DescribeIndexResponse' {} a -> s {schema = a} :: DescribeIndexResponse)

-- | The response's http status code.
describeIndexResponse_httpStatus :: Lens.Lens' DescribeIndexResponse Prelude.Int
describeIndexResponse_httpStatus = Lens.lens (\DescribeIndexResponse' {httpStatus} -> httpStatus) (\s@DescribeIndexResponse' {} a -> s {httpStatus = a} :: DescribeIndexResponse)

instance Prelude.NFData DescribeIndexResponse where
  rnf DescribeIndexResponse' {..} =
    Prelude.rnf indexName
      `Prelude.seq` Prelude.rnf indexStatus
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf httpStatus
