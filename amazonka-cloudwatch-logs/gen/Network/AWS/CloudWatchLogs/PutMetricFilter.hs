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
-- Module      : Network.AWS.CloudWatchLogs.PutMetricFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a metric filter and associates it with the specified
-- log group. Metric filters allow you to configure rules to extract metric
-- data from log events ingested through
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html PutLogEvents>.
--
-- The maximum number of metric filters that can be associated with a log
-- group is 100.
module Network.AWS.CloudWatchLogs.PutMetricFilter
  ( -- * Creating a Request
    PutMetricFilter (..),
    newPutMetricFilter,

    -- * Request Lenses
    putMetricFilter_logGroupName,
    putMetricFilter_filterName,
    putMetricFilter_filterPattern,
    putMetricFilter_metricTransformations,

    -- * Destructuring the Response
    PutMetricFilterResponse (..),
    newPutMetricFilterResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutMetricFilter' smart constructor.
data PutMetricFilter = PutMetricFilter'
  { -- | The name of the log group.
    logGroupName :: Core.Text,
    -- | A name for the metric filter.
    filterName :: Core.Text,
    -- | A filter pattern for extracting metric data out of ingested log events.
    filterPattern :: Core.Text,
    -- | A collection of information that defines how metric data gets emitted.
    metricTransformations :: Core.NonEmpty MetricTransformation
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutMetricFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logGroupName', 'putMetricFilter_logGroupName' - The name of the log group.
--
-- 'filterName', 'putMetricFilter_filterName' - A name for the metric filter.
--
-- 'filterPattern', 'putMetricFilter_filterPattern' - A filter pattern for extracting metric data out of ingested log events.
--
-- 'metricTransformations', 'putMetricFilter_metricTransformations' - A collection of information that defines how metric data gets emitted.
newPutMetricFilter ::
  -- | 'logGroupName'
  Core.Text ->
  -- | 'filterName'
  Core.Text ->
  -- | 'filterPattern'
  Core.Text ->
  -- | 'metricTransformations'
  Core.NonEmpty MetricTransformation ->
  PutMetricFilter
newPutMetricFilter
  pLogGroupName_
  pFilterName_
  pFilterPattern_
  pMetricTransformations_ =
    PutMetricFilter'
      { logGroupName = pLogGroupName_,
        filterName = pFilterName_,
        filterPattern = pFilterPattern_,
        metricTransformations =
          Lens._Coerce Lens.# pMetricTransformations_
      }

-- | The name of the log group.
putMetricFilter_logGroupName :: Lens.Lens' PutMetricFilter Core.Text
putMetricFilter_logGroupName = Lens.lens (\PutMetricFilter' {logGroupName} -> logGroupName) (\s@PutMetricFilter' {} a -> s {logGroupName = a} :: PutMetricFilter)

-- | A name for the metric filter.
putMetricFilter_filterName :: Lens.Lens' PutMetricFilter Core.Text
putMetricFilter_filterName = Lens.lens (\PutMetricFilter' {filterName} -> filterName) (\s@PutMetricFilter' {} a -> s {filterName = a} :: PutMetricFilter)

-- | A filter pattern for extracting metric data out of ingested log events.
putMetricFilter_filterPattern :: Lens.Lens' PutMetricFilter Core.Text
putMetricFilter_filterPattern = Lens.lens (\PutMetricFilter' {filterPattern} -> filterPattern) (\s@PutMetricFilter' {} a -> s {filterPattern = a} :: PutMetricFilter)

-- | A collection of information that defines how metric data gets emitted.
putMetricFilter_metricTransformations :: Lens.Lens' PutMetricFilter (Core.NonEmpty MetricTransformation)
putMetricFilter_metricTransformations = Lens.lens (\PutMetricFilter' {metricTransformations} -> metricTransformations) (\s@PutMetricFilter' {} a -> s {metricTransformations = a} :: PutMetricFilter) Core.. Lens._Coerce

instance Core.AWSRequest PutMetricFilter where
  type
    AWSResponse PutMetricFilter =
      PutMetricFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutMetricFilterResponse'

instance Core.Hashable PutMetricFilter

instance Core.NFData PutMetricFilter

instance Core.ToHeaders PutMetricFilter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.PutMetricFilter" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutMetricFilter where
  toJSON PutMetricFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("logGroupName" Core..= logGroupName),
            Core.Just ("filterName" Core..= filterName),
            Core.Just ("filterPattern" Core..= filterPattern),
            Core.Just
              ( "metricTransformations"
                  Core..= metricTransformations
              )
          ]
      )

instance Core.ToPath PutMetricFilter where
  toPath = Core.const "/"

instance Core.ToQuery PutMetricFilter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutMetricFilterResponse' smart constructor.
data PutMetricFilterResponse = PutMetricFilterResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutMetricFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutMetricFilterResponse ::
  PutMetricFilterResponse
newPutMetricFilterResponse = PutMetricFilterResponse'

instance Core.NFData PutMetricFilterResponse
