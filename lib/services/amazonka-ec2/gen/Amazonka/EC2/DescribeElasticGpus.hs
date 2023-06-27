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
-- Module      : Amazonka.EC2.DescribeElasticGpus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Elastic Graphics accelerator associated with your
-- instances. For more information about Elastic Graphics, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon Elastic Graphics>.
module Amazonka.EC2.DescribeElasticGpus
  ( -- * Creating a Request
    DescribeElasticGpus (..),
    newDescribeElasticGpus,

    -- * Request Lenses
    describeElasticGpus_dryRun,
    describeElasticGpus_elasticGpuIds,
    describeElasticGpus_filters,
    describeElasticGpus_maxResults,
    describeElasticGpus_nextToken,

    -- * Destructuring the Response
    DescribeElasticGpusResponse (..),
    newDescribeElasticGpusResponse,

    -- * Response Lenses
    describeElasticGpusResponse_elasticGpuSet,
    describeElasticGpusResponse_maxResults,
    describeElasticGpusResponse_nextToken,
    describeElasticGpusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeElasticGpus' smart constructor.
data DescribeElasticGpus = DescribeElasticGpus'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Elastic Graphics accelerator IDs.
    elasticGpuIds :: Prelude.Maybe [Prelude.Text],
    -- | The filters.
    --
    -- -   @availability-zone@ - The Availability Zone in which the Elastic
    --     Graphics accelerator resides.
    --
    -- -   @elastic-gpu-health@ - The status of the Elastic Graphics
    --     accelerator (@OK@ | @IMPAIRED@).
    --
    -- -   @elastic-gpu-state@ - The state of the Elastic Graphics accelerator
    --     (@ATTACHED@).
    --
    -- -   @elastic-gpu-type@ - The type of Elastic Graphics accelerator; for
    --     example, @eg1.medium@.
    --
    -- -   @instance-id@ - The ID of the instance to which the Elastic Graphics
    --     accelerator is associated.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. This value can be between 5 and 1000.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to request the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticGpus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeElasticGpus_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'elasticGpuIds', 'describeElasticGpus_elasticGpuIds' - The Elastic Graphics accelerator IDs.
--
-- 'filters', 'describeElasticGpus_filters' - The filters.
--
-- -   @availability-zone@ - The Availability Zone in which the Elastic
--     Graphics accelerator resides.
--
-- -   @elastic-gpu-health@ - The status of the Elastic Graphics
--     accelerator (@OK@ | @IMPAIRED@).
--
-- -   @elastic-gpu-state@ - The state of the Elastic Graphics accelerator
--     (@ATTACHED@).
--
-- -   @elastic-gpu-type@ - The type of Elastic Graphics accelerator; for
--     example, @eg1.medium@.
--
-- -   @instance-id@ - The ID of the instance to which the Elastic Graphics
--     accelerator is associated.
--
-- 'maxResults', 'describeElasticGpus_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 5 and 1000.
--
-- 'nextToken', 'describeElasticGpus_nextToken' - The token to request the next page of results.
newDescribeElasticGpus ::
  DescribeElasticGpus
newDescribeElasticGpus =
  DescribeElasticGpus'
    { dryRun = Prelude.Nothing,
      elasticGpuIds = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeElasticGpus_dryRun :: Lens.Lens' DescribeElasticGpus (Prelude.Maybe Prelude.Bool)
describeElasticGpus_dryRun = Lens.lens (\DescribeElasticGpus' {dryRun} -> dryRun) (\s@DescribeElasticGpus' {} a -> s {dryRun = a} :: DescribeElasticGpus)

-- | The Elastic Graphics accelerator IDs.
describeElasticGpus_elasticGpuIds :: Lens.Lens' DescribeElasticGpus (Prelude.Maybe [Prelude.Text])
describeElasticGpus_elasticGpuIds = Lens.lens (\DescribeElasticGpus' {elasticGpuIds} -> elasticGpuIds) (\s@DescribeElasticGpus' {} a -> s {elasticGpuIds = a} :: DescribeElasticGpus) Prelude.. Lens.mapping Lens.coerced

-- | The filters.
--
-- -   @availability-zone@ - The Availability Zone in which the Elastic
--     Graphics accelerator resides.
--
-- -   @elastic-gpu-health@ - The status of the Elastic Graphics
--     accelerator (@OK@ | @IMPAIRED@).
--
-- -   @elastic-gpu-state@ - The state of the Elastic Graphics accelerator
--     (@ATTACHED@).
--
-- -   @elastic-gpu-type@ - The type of Elastic Graphics accelerator; for
--     example, @eg1.medium@.
--
-- -   @instance-id@ - The ID of the instance to which the Elastic Graphics
--     accelerator is associated.
describeElasticGpus_filters :: Lens.Lens' DescribeElasticGpus (Prelude.Maybe [Filter])
describeElasticGpus_filters = Lens.lens (\DescribeElasticGpus' {filters} -> filters) (\s@DescribeElasticGpus' {} a -> s {filters = a} :: DescribeElasticGpus) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 5 and 1000.
describeElasticGpus_maxResults :: Lens.Lens' DescribeElasticGpus (Prelude.Maybe Prelude.Natural)
describeElasticGpus_maxResults = Lens.lens (\DescribeElasticGpus' {maxResults} -> maxResults) (\s@DescribeElasticGpus' {} a -> s {maxResults = a} :: DescribeElasticGpus)

-- | The token to request the next page of results.
describeElasticGpus_nextToken :: Lens.Lens' DescribeElasticGpus (Prelude.Maybe Prelude.Text)
describeElasticGpus_nextToken = Lens.lens (\DescribeElasticGpus' {nextToken} -> nextToken) (\s@DescribeElasticGpus' {} a -> s {nextToken = a} :: DescribeElasticGpus)

instance Core.AWSRequest DescribeElasticGpus where
  type
    AWSResponse DescribeElasticGpus =
      DescribeElasticGpusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeElasticGpusResponse'
            Prelude.<$> ( x
                            Data..@? "elasticGpuSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (x Data..@? "maxResults")
            Prelude.<*> (x Data..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeElasticGpus where
  hashWithSalt _salt DescribeElasticGpus' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` elasticGpuIds
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeElasticGpus where
  rnf DescribeElasticGpus' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf elasticGpuIds
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeElasticGpus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeElasticGpus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeElasticGpus where
  toQuery DescribeElasticGpus' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeElasticGpus" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "ElasticGpuId"
              Prelude.<$> elasticGpuIds
          ),
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeElasticGpusResponse' smart constructor.
data DescribeElasticGpusResponse = DescribeElasticGpusResponse'
  { -- | Information about the Elastic Graphics accelerators.
    elasticGpuSet :: Prelude.Maybe [ElasticGpus],
    -- | The total number of items to return. If the total number of items
    -- available is more than the value specified in max-items then a
    -- Next-Token will be provided in the output that you can use to resume
    -- pagination.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeElasticGpusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'elasticGpuSet', 'describeElasticGpusResponse_elasticGpuSet' - Information about the Elastic Graphics accelerators.
--
-- 'maxResults', 'describeElasticGpusResponse_maxResults' - The total number of items to return. If the total number of items
-- available is more than the value specified in max-items then a
-- Next-Token will be provided in the output that you can use to resume
-- pagination.
--
-- 'nextToken', 'describeElasticGpusResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeElasticGpusResponse_httpStatus' - The response's http status code.
newDescribeElasticGpusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeElasticGpusResponse
newDescribeElasticGpusResponse pHttpStatus_ =
  DescribeElasticGpusResponse'
    { elasticGpuSet =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Elastic Graphics accelerators.
describeElasticGpusResponse_elasticGpuSet :: Lens.Lens' DescribeElasticGpusResponse (Prelude.Maybe [ElasticGpus])
describeElasticGpusResponse_elasticGpuSet = Lens.lens (\DescribeElasticGpusResponse' {elasticGpuSet} -> elasticGpuSet) (\s@DescribeElasticGpusResponse' {} a -> s {elasticGpuSet = a} :: DescribeElasticGpusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of items to return. If the total number of items
-- available is more than the value specified in max-items then a
-- Next-Token will be provided in the output that you can use to resume
-- pagination.
describeElasticGpusResponse_maxResults :: Lens.Lens' DescribeElasticGpusResponse (Prelude.Maybe Prelude.Int)
describeElasticGpusResponse_maxResults = Lens.lens (\DescribeElasticGpusResponse' {maxResults} -> maxResults) (\s@DescribeElasticGpusResponse' {} a -> s {maxResults = a} :: DescribeElasticGpusResponse)

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeElasticGpusResponse_nextToken :: Lens.Lens' DescribeElasticGpusResponse (Prelude.Maybe Prelude.Text)
describeElasticGpusResponse_nextToken = Lens.lens (\DescribeElasticGpusResponse' {nextToken} -> nextToken) (\s@DescribeElasticGpusResponse' {} a -> s {nextToken = a} :: DescribeElasticGpusResponse)

-- | The response's http status code.
describeElasticGpusResponse_httpStatus :: Lens.Lens' DescribeElasticGpusResponse Prelude.Int
describeElasticGpusResponse_httpStatus = Lens.lens (\DescribeElasticGpusResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticGpusResponse' {} a -> s {httpStatus = a} :: DescribeElasticGpusResponse)

instance Prelude.NFData DescribeElasticGpusResponse where
  rnf DescribeElasticGpusResponse' {..} =
    Prelude.rnf elasticGpuSet
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
