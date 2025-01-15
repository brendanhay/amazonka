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
-- Module      : Amazonka.EC2.DescribeVolumeStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the specified volumes. Volume status provides
-- the result of the checks performed on your volumes to determine events
-- that can impair the performance of your volumes. The performance of a
-- volume can be affected if an issue occurs on the volume\'s underlying
-- host. If the volume\'s underlying host experiences a power outage or
-- system issue, after the system is restored, there could be data
-- inconsistencies on the volume. Volume events notify you if this occurs.
-- Volume actions notify you if any action needs to be taken in response to
-- the event.
--
-- The @DescribeVolumeStatus@ operation provides the following information
-- about the specified volumes:
--
-- /Status/: Reflects the current status of the volume. The possible values
-- are @ok@, @impaired@ , @warning@, or @insufficient-data@. If all checks
-- pass, the overall status of the volume is @ok@. If the check fails, the
-- overall status is @impaired@. If the status is @insufficient-data@, then
-- the checks might still be taking place on your volume at the time. We
-- recommend that you retry the request. For more information about volume
-- status, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-volume-status.html Monitor the status of your volumes>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /Events/: Reflect the cause of a volume status and might require you to
-- take action. For example, if your volume returns an @impaired@ status,
-- then the volume event might be @potential-data-inconsistency@. This
-- means that your volume has been affected by an issue with the underlying
-- host, has all I\/O operations disabled, and might have inconsistent
-- data.
--
-- /Actions/: Reflect the actions you might have to take in response to an
-- event. For example, if the status of the volume is @impaired@ and the
-- volume event shows @potential-data-inconsistency@, then the action shows
-- @enable-volume-io@. This means that you may want to enable the I\/O
-- operations for the volume by calling the EnableVolumeIO action and then
-- check the volume for data consistency.
--
-- Volume status is based on the volume status checks, and does not reflect
-- the volume state. Therefore, volume status does not indicate volumes in
-- the @error@ state (for example, when a volume is incapable of accepting
-- I\/O.)
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVolumeStatus
  ( -- * Creating a Request
    DescribeVolumeStatus (..),
    newDescribeVolumeStatus,

    -- * Request Lenses
    describeVolumeStatus_dryRun,
    describeVolumeStatus_filters,
    describeVolumeStatus_maxResults,
    describeVolumeStatus_nextToken,
    describeVolumeStatus_volumeIds,

    -- * Destructuring the Response
    DescribeVolumeStatusResponse (..),
    newDescribeVolumeStatusResponse,

    -- * Response Lenses
    describeVolumeStatusResponse_nextToken,
    describeVolumeStatusResponse_volumeStatuses,
    describeVolumeStatusResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVolumeStatus' smart constructor.
data DescribeVolumeStatus = DescribeVolumeStatus'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters.
    --
    -- -   @action.code@ - The action code for the event (for example,
    --     @enable-volume-io@).
    --
    -- -   @action.description@ - A description of the action.
    --
    -- -   @action.event-id@ - The event ID associated with the action.
    --
    -- -   @availability-zone@ - The Availability Zone of the instance.
    --
    -- -   @event.description@ - A description of the event.
    --
    -- -   @event.event-id@ - The event ID.
    --
    -- -   @event.event-type@ - The event type (for @io-enabled@: @passed@ |
    --     @failed@; for @io-performance@: @io-performance:degraded@ |
    --     @io-performance:severely-degraded@ | @io-performance:stalled@).
    --
    -- -   @event.not-after@ - The latest end time for the event.
    --
    -- -   @event.not-before@ - The earliest start time for the event.
    --
    -- -   @volume-status.details-name@ - The cause for @volume-status.status@
    --     (@io-enabled@ | @io-performance@).
    --
    -- -   @volume-status.details-status@ - The status of
    --     @volume-status.details-name@ (for @io-enabled@: @passed@ | @failed@;
    --     for @io-performance@: @normal@ | @degraded@ | @severely-degraded@ |
    --     @stalled@).
    --
    -- -   @volume-status.status@ - The status of the volume (@ok@ | @impaired@
    --     | @warning@ | @insufficient-data@).
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of volume results returned by @DescribeVolumeStatus@
    -- in paginated output. When this parameter is used, the request only
    -- returns @MaxResults@ results in a single page along with a @NextToken@
    -- response element. The remaining results of the initial request can be
    -- seen by sending another request with the returned @NextToken@ value.
    -- This value can be between 5 and 1,000; if @MaxResults@ is given a value
    -- larger than 1,000, only 1,000 results are returned. If this parameter is
    -- not used, then @DescribeVolumeStatus@ returns all results. You cannot
    -- specify this parameter and the volume IDs parameter in the same request.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The @NextToken@ value to include in a future @DescribeVolumeStatus@
    -- request. When the results of the request exceed @MaxResults@, this value
    -- can be used to retrieve the next page of results. This value is @null@
    -- when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the volumes.
    --
    -- Default: Describes all your volumes.
    volumeIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVolumeStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVolumeStatus_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVolumeStatus_filters' - The filters.
--
-- -   @action.code@ - The action code for the event (for example,
--     @enable-volume-io@).
--
-- -   @action.description@ - A description of the action.
--
-- -   @action.event-id@ - The event ID associated with the action.
--
-- -   @availability-zone@ - The Availability Zone of the instance.
--
-- -   @event.description@ - A description of the event.
--
-- -   @event.event-id@ - The event ID.
--
-- -   @event.event-type@ - The event type (for @io-enabled@: @passed@ |
--     @failed@; for @io-performance@: @io-performance:degraded@ |
--     @io-performance:severely-degraded@ | @io-performance:stalled@).
--
-- -   @event.not-after@ - The latest end time for the event.
--
-- -   @event.not-before@ - The earliest start time for the event.
--
-- -   @volume-status.details-name@ - The cause for @volume-status.status@
--     (@io-enabled@ | @io-performance@).
--
-- -   @volume-status.details-status@ - The status of
--     @volume-status.details-name@ (for @io-enabled@: @passed@ | @failed@;
--     for @io-performance@: @normal@ | @degraded@ | @severely-degraded@ |
--     @stalled@).
--
-- -   @volume-status.status@ - The status of the volume (@ok@ | @impaired@
--     | @warning@ | @insufficient-data@).
--
-- 'maxResults', 'describeVolumeStatus_maxResults' - The maximum number of volume results returned by @DescribeVolumeStatus@
-- in paginated output. When this parameter is used, the request only
-- returns @MaxResults@ results in a single page along with a @NextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another request with the returned @NextToken@ value.
-- This value can be between 5 and 1,000; if @MaxResults@ is given a value
-- larger than 1,000, only 1,000 results are returned. If this parameter is
-- not used, then @DescribeVolumeStatus@ returns all results. You cannot
-- specify this parameter and the volume IDs parameter in the same request.
--
-- 'nextToken', 'describeVolumeStatus_nextToken' - The @NextToken@ value to include in a future @DescribeVolumeStatus@
-- request. When the results of the request exceed @MaxResults@, this value
-- can be used to retrieve the next page of results. This value is @null@
-- when there are no more results to return.
--
-- 'volumeIds', 'describeVolumeStatus_volumeIds' - The IDs of the volumes.
--
-- Default: Describes all your volumes.
newDescribeVolumeStatus ::
  DescribeVolumeStatus
newDescribeVolumeStatus =
  DescribeVolumeStatus'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      volumeIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVolumeStatus_dryRun :: Lens.Lens' DescribeVolumeStatus (Prelude.Maybe Prelude.Bool)
describeVolumeStatus_dryRun = Lens.lens (\DescribeVolumeStatus' {dryRun} -> dryRun) (\s@DescribeVolumeStatus' {} a -> s {dryRun = a} :: DescribeVolumeStatus)

-- | The filters.
--
-- -   @action.code@ - The action code for the event (for example,
--     @enable-volume-io@).
--
-- -   @action.description@ - A description of the action.
--
-- -   @action.event-id@ - The event ID associated with the action.
--
-- -   @availability-zone@ - The Availability Zone of the instance.
--
-- -   @event.description@ - A description of the event.
--
-- -   @event.event-id@ - The event ID.
--
-- -   @event.event-type@ - The event type (for @io-enabled@: @passed@ |
--     @failed@; for @io-performance@: @io-performance:degraded@ |
--     @io-performance:severely-degraded@ | @io-performance:stalled@).
--
-- -   @event.not-after@ - The latest end time for the event.
--
-- -   @event.not-before@ - The earliest start time for the event.
--
-- -   @volume-status.details-name@ - The cause for @volume-status.status@
--     (@io-enabled@ | @io-performance@).
--
-- -   @volume-status.details-status@ - The status of
--     @volume-status.details-name@ (for @io-enabled@: @passed@ | @failed@;
--     for @io-performance@: @normal@ | @degraded@ | @severely-degraded@ |
--     @stalled@).
--
-- -   @volume-status.status@ - The status of the volume (@ok@ | @impaired@
--     | @warning@ | @insufficient-data@).
describeVolumeStatus_filters :: Lens.Lens' DescribeVolumeStatus (Prelude.Maybe [Filter])
describeVolumeStatus_filters = Lens.lens (\DescribeVolumeStatus' {filters} -> filters) (\s@DescribeVolumeStatus' {} a -> s {filters = a} :: DescribeVolumeStatus) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of volume results returned by @DescribeVolumeStatus@
-- in paginated output. When this parameter is used, the request only
-- returns @MaxResults@ results in a single page along with a @NextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another request with the returned @NextToken@ value.
-- This value can be between 5 and 1,000; if @MaxResults@ is given a value
-- larger than 1,000, only 1,000 results are returned. If this parameter is
-- not used, then @DescribeVolumeStatus@ returns all results. You cannot
-- specify this parameter and the volume IDs parameter in the same request.
describeVolumeStatus_maxResults :: Lens.Lens' DescribeVolumeStatus (Prelude.Maybe Prelude.Int)
describeVolumeStatus_maxResults = Lens.lens (\DescribeVolumeStatus' {maxResults} -> maxResults) (\s@DescribeVolumeStatus' {} a -> s {maxResults = a} :: DescribeVolumeStatus)

-- | The @NextToken@ value to include in a future @DescribeVolumeStatus@
-- request. When the results of the request exceed @MaxResults@, this value
-- can be used to retrieve the next page of results. This value is @null@
-- when there are no more results to return.
describeVolumeStatus_nextToken :: Lens.Lens' DescribeVolumeStatus (Prelude.Maybe Prelude.Text)
describeVolumeStatus_nextToken = Lens.lens (\DescribeVolumeStatus' {nextToken} -> nextToken) (\s@DescribeVolumeStatus' {} a -> s {nextToken = a} :: DescribeVolumeStatus)

-- | The IDs of the volumes.
--
-- Default: Describes all your volumes.
describeVolumeStatus_volumeIds :: Lens.Lens' DescribeVolumeStatus (Prelude.Maybe [Prelude.Text])
describeVolumeStatus_volumeIds = Lens.lens (\DescribeVolumeStatus' {volumeIds} -> volumeIds) (\s@DescribeVolumeStatus' {} a -> s {volumeIds = a} :: DescribeVolumeStatus) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeVolumeStatus where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVolumeStatusResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVolumeStatusResponse_volumeStatuses
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& describeVolumeStatus_nextToken
              Lens..~ rs
              Lens.^? describeVolumeStatusResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest DescribeVolumeStatus where
  type
    AWSResponse DescribeVolumeStatus =
      DescribeVolumeStatusResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVolumeStatusResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x Data..@? "volumeStatusSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVolumeStatus where
  hashWithSalt _salt DescribeVolumeStatus' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` volumeIds

instance Prelude.NFData DescribeVolumeStatus where
  rnf DescribeVolumeStatus' {..} =
    Prelude.rnf dryRun `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nextToken `Prelude.seq`
            Prelude.rnf volumeIds

instance Data.ToHeaders DescribeVolumeStatus where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVolumeStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVolumeStatus where
  toQuery DescribeVolumeStatus' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeVolumeStatus" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "VolumeId" Prelude.<$> volumeIds)
      ]

-- | /See:/ 'newDescribeVolumeStatusResponse' smart constructor.
data DescribeVolumeStatusResponse = DescribeVolumeStatusResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the status of the volumes.
    volumeStatuses :: Prelude.Maybe [VolumeStatusItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVolumeStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVolumeStatusResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'volumeStatuses', 'describeVolumeStatusResponse_volumeStatuses' - Information about the status of the volumes.
--
-- 'httpStatus', 'describeVolumeStatusResponse_httpStatus' - The response's http status code.
newDescribeVolumeStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVolumeStatusResponse
newDescribeVolumeStatusResponse pHttpStatus_ =
  DescribeVolumeStatusResponse'
    { nextToken =
        Prelude.Nothing,
      volumeStatuses = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVolumeStatusResponse_nextToken :: Lens.Lens' DescribeVolumeStatusResponse (Prelude.Maybe Prelude.Text)
describeVolumeStatusResponse_nextToken = Lens.lens (\DescribeVolumeStatusResponse' {nextToken} -> nextToken) (\s@DescribeVolumeStatusResponse' {} a -> s {nextToken = a} :: DescribeVolumeStatusResponse)

-- | Information about the status of the volumes.
describeVolumeStatusResponse_volumeStatuses :: Lens.Lens' DescribeVolumeStatusResponse (Prelude.Maybe [VolumeStatusItem])
describeVolumeStatusResponse_volumeStatuses = Lens.lens (\DescribeVolumeStatusResponse' {volumeStatuses} -> volumeStatuses) (\s@DescribeVolumeStatusResponse' {} a -> s {volumeStatuses = a} :: DescribeVolumeStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVolumeStatusResponse_httpStatus :: Lens.Lens' DescribeVolumeStatusResponse Prelude.Int
describeVolumeStatusResponse_httpStatus = Lens.lens (\DescribeVolumeStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeVolumeStatusResponse' {} a -> s {httpStatus = a} :: DescribeVolumeStatusResponse)

instance Prelude.NFData DescribeVolumeStatusResponse where
  rnf DescribeVolumeStatusResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf volumeStatuses `Prelude.seq`
        Prelude.rnf httpStatus
