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
-- Module      : Network.AWS.EC2.DescribeElasticGpus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the Elastic Graphics accelerator associated with your
-- instances. For more information about Elastic Graphics, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/elastic-graphics.html Amazon Elastic Graphics>.
module Network.AWS.EC2.DescribeElasticGpus
  ( -- * Creating a Request
    DescribeElasticGpus (..),
    newDescribeElasticGpus,

    -- * Request Lenses
    describeElasticGpus_nextToken,
    describeElasticGpus_elasticGpuIds,
    describeElasticGpus_dryRun,
    describeElasticGpus_maxResults,
    describeElasticGpus_filters,

    -- * Destructuring the Response
    DescribeElasticGpusResponse (..),
    newDescribeElasticGpusResponse,

    -- * Response Lenses
    describeElasticGpusResponse_nextToken,
    describeElasticGpusResponse_maxResults,
    describeElasticGpusResponse_elasticGpuSet,
    describeElasticGpusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeElasticGpus' smart constructor.
data DescribeElasticGpus = DescribeElasticGpus'
  { -- | The token to request the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | The Elastic Graphics accelerator IDs.
    elasticGpuIds :: Core.Maybe [Core.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return in a single call. To retrieve
    -- the remaining results, make another call with the returned @NextToken@
    -- value. This value can be between 5 and 1000.
    maxResults :: Core.Maybe Core.Natural,
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
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeElasticGpus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeElasticGpus_nextToken' - The token to request the next page of results.
--
-- 'elasticGpuIds', 'describeElasticGpus_elasticGpuIds' - The Elastic Graphics accelerator IDs.
--
-- 'dryRun', 'describeElasticGpus_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeElasticGpus_maxResults' - The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 5 and 1000.
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
newDescribeElasticGpus ::
  DescribeElasticGpus
newDescribeElasticGpus =
  DescribeElasticGpus'
    { nextToken = Core.Nothing,
      elasticGpuIds = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token to request the next page of results.
describeElasticGpus_nextToken :: Lens.Lens' DescribeElasticGpus (Core.Maybe Core.Text)
describeElasticGpus_nextToken = Lens.lens (\DescribeElasticGpus' {nextToken} -> nextToken) (\s@DescribeElasticGpus' {} a -> s {nextToken = a} :: DescribeElasticGpus)

-- | The Elastic Graphics accelerator IDs.
describeElasticGpus_elasticGpuIds :: Lens.Lens' DescribeElasticGpus (Core.Maybe [Core.Text])
describeElasticGpus_elasticGpuIds = Lens.lens (\DescribeElasticGpus' {elasticGpuIds} -> elasticGpuIds) (\s@DescribeElasticGpus' {} a -> s {elasticGpuIds = a} :: DescribeElasticGpus) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeElasticGpus_dryRun :: Lens.Lens' DescribeElasticGpus (Core.Maybe Core.Bool)
describeElasticGpus_dryRun = Lens.lens (\DescribeElasticGpus' {dryRun} -> dryRun) (\s@DescribeElasticGpus' {} a -> s {dryRun = a} :: DescribeElasticGpus)

-- | The maximum number of results to return in a single call. To retrieve
-- the remaining results, make another call with the returned @NextToken@
-- value. This value can be between 5 and 1000.
describeElasticGpus_maxResults :: Lens.Lens' DescribeElasticGpus (Core.Maybe Core.Natural)
describeElasticGpus_maxResults = Lens.lens (\DescribeElasticGpus' {maxResults} -> maxResults) (\s@DescribeElasticGpus' {} a -> s {maxResults = a} :: DescribeElasticGpus)

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
describeElasticGpus_filters :: Lens.Lens' DescribeElasticGpus (Core.Maybe [Filter])
describeElasticGpus_filters = Lens.lens (\DescribeElasticGpus' {filters} -> filters) (\s@DescribeElasticGpus' {} a -> s {filters = a} :: DescribeElasticGpus) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeElasticGpus where
  type
    AWSResponse DescribeElasticGpus =
      DescribeElasticGpusResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeElasticGpusResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> (x Core..@? "maxResults")
            Core.<*> ( x Core..@? "elasticGpuSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeElasticGpus

instance Core.NFData DescribeElasticGpus

instance Core.ToHeaders DescribeElasticGpus where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeElasticGpus where
  toPath = Core.const "/"

instance Core.ToQuery DescribeElasticGpus where
  toQuery DescribeElasticGpus' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeElasticGpus" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "ElasticGpuId"
              Core.<$> elasticGpuIds
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeElasticGpusResponse' smart constructor.
data DescribeElasticGpusResponse = DescribeElasticGpusResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of items to return. If the total number of items
    -- available is more than the value specified in max-items then a
    -- Next-Token will be provided in the output that you can use to resume
    -- pagination.
    maxResults :: Core.Maybe Core.Int,
    -- | Information about the Elastic Graphics accelerators.
    elasticGpuSet :: Core.Maybe [ElasticGpus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeElasticGpusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeElasticGpusResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'maxResults', 'describeElasticGpusResponse_maxResults' - The total number of items to return. If the total number of items
-- available is more than the value specified in max-items then a
-- Next-Token will be provided in the output that you can use to resume
-- pagination.
--
-- 'elasticGpuSet', 'describeElasticGpusResponse_elasticGpuSet' - Information about the Elastic Graphics accelerators.
--
-- 'httpStatus', 'describeElasticGpusResponse_httpStatus' - The response's http status code.
newDescribeElasticGpusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeElasticGpusResponse
newDescribeElasticGpusResponse pHttpStatus_ =
  DescribeElasticGpusResponse'
    { nextToken =
        Core.Nothing,
      maxResults = Core.Nothing,
      elasticGpuSet = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeElasticGpusResponse_nextToken :: Lens.Lens' DescribeElasticGpusResponse (Core.Maybe Core.Text)
describeElasticGpusResponse_nextToken = Lens.lens (\DescribeElasticGpusResponse' {nextToken} -> nextToken) (\s@DescribeElasticGpusResponse' {} a -> s {nextToken = a} :: DescribeElasticGpusResponse)

-- | The total number of items to return. If the total number of items
-- available is more than the value specified in max-items then a
-- Next-Token will be provided in the output that you can use to resume
-- pagination.
describeElasticGpusResponse_maxResults :: Lens.Lens' DescribeElasticGpusResponse (Core.Maybe Core.Int)
describeElasticGpusResponse_maxResults = Lens.lens (\DescribeElasticGpusResponse' {maxResults} -> maxResults) (\s@DescribeElasticGpusResponse' {} a -> s {maxResults = a} :: DescribeElasticGpusResponse)

-- | Information about the Elastic Graphics accelerators.
describeElasticGpusResponse_elasticGpuSet :: Lens.Lens' DescribeElasticGpusResponse (Core.Maybe [ElasticGpus])
describeElasticGpusResponse_elasticGpuSet = Lens.lens (\DescribeElasticGpusResponse' {elasticGpuSet} -> elasticGpuSet) (\s@DescribeElasticGpusResponse' {} a -> s {elasticGpuSet = a} :: DescribeElasticGpusResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeElasticGpusResponse_httpStatus :: Lens.Lens' DescribeElasticGpusResponse Core.Int
describeElasticGpusResponse_httpStatus = Lens.lens (\DescribeElasticGpusResponse' {httpStatus} -> httpStatus) (\s@DescribeElasticGpusResponse' {} a -> s {httpStatus = a} :: DescribeElasticGpusResponse)

instance Core.NFData DescribeElasticGpusResponse
