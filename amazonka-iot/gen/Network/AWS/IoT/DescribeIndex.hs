{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.DescribeIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a search index.
module Network.AWS.IoT.DescribeIndex
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
    describeIndexResponse_schema,
    describeIndexResponse_indexStatus,
    describeIndexResponse_httpStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeIndex' smart constructor.
data DescribeIndex = DescribeIndex'
  { -- | The index name.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeIndex where
  type Rs DescribeIndex = DescribeIndexResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeIndexResponse'
            Prelude.<$> (x Prelude..?> "indexName")
            Prelude.<*> (x Prelude..?> "schema")
            Prelude.<*> (x Prelude..?> "indexStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeIndex

instance Prelude.NFData DescribeIndex

instance Prelude.ToHeaders DescribeIndex where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DescribeIndex where
  toPath DescribeIndex' {..} =
    Prelude.mconcat
      ["/indices/", Prelude.toBS indexName]

instance Prelude.ToQuery DescribeIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeIndexResponse' smart constructor.
data DescribeIndexResponse = DescribeIndexResponse'
  { -- | The index name.
    indexName :: Prelude.Maybe Prelude.Text,
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
    schema :: Prelude.Maybe Prelude.Text,
    -- | The index status.
    indexStatus :: Prelude.Maybe IndexStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'indexStatus', 'describeIndexResponse_indexStatus' - The index status.
--
-- 'httpStatus', 'describeIndexResponse_httpStatus' - The response's http status code.
newDescribeIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeIndexResponse
newDescribeIndexResponse pHttpStatus_ =
  DescribeIndexResponse'
    { indexName = Prelude.Nothing,
      schema = Prelude.Nothing,
      indexStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The index name.
describeIndexResponse_indexName :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_indexName = Lens.lens (\DescribeIndexResponse' {indexName} -> indexName) (\s@DescribeIndexResponse' {} a -> s {indexName = a} :: DescribeIndexResponse)

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
describeIndexResponse_schema :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe Prelude.Text)
describeIndexResponse_schema = Lens.lens (\DescribeIndexResponse' {schema} -> schema) (\s@DescribeIndexResponse' {} a -> s {schema = a} :: DescribeIndexResponse)

-- | The index status.
describeIndexResponse_indexStatus :: Lens.Lens' DescribeIndexResponse (Prelude.Maybe IndexStatus)
describeIndexResponse_indexStatus = Lens.lens (\DescribeIndexResponse' {indexStatus} -> indexStatus) (\s@DescribeIndexResponse' {} a -> s {indexStatus = a} :: DescribeIndexResponse)

-- | The response's http status code.
describeIndexResponse_httpStatus :: Lens.Lens' DescribeIndexResponse Prelude.Int
describeIndexResponse_httpStatus = Lens.lens (\DescribeIndexResponse' {httpStatus} -> httpStatus) (\s@DescribeIndexResponse' {} a -> s {httpStatus = a} :: DescribeIndexResponse)

instance Prelude.NFData DescribeIndexResponse
