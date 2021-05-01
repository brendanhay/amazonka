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
-- Module      : Network.AWS.CloudWatchLogs.PutSubscriptionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a subscription filter and associates it with the
-- specified log group. Subscription filters allow you to subscribe to a
-- real-time stream of log events ingested through
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html PutLogEvents>
-- and have them delivered to a specific destination. When log events are
-- sent to the receiving service, they are Base64 encoded and compressed
-- with the gzip format.
--
-- The following destinations are supported for subscription filters:
--
-- -   An Amazon Kinesis stream belonging to the same account as the
--     subscription filter, for same-account delivery.
--
-- -   A logical destination that belongs to a different account, for
--     cross-account delivery.
--
-- -   An Amazon Kinesis Firehose delivery stream that belongs to the same
--     account as the subscription filter, for same-account delivery.
--
-- -   An AWS Lambda function that belongs to the same account as the
--     subscription filter, for same-account delivery.
--
-- There can only be one subscription filter associated with a log group.
-- If you are updating an existing filter, you must specify the correct
-- name in @filterName@. Otherwise, the call fails because you cannot
-- associate a second filter with a log group.
--
-- To perform a @PutSubscriptionFilter@ operation, you must also have the
-- @iam:PassRole@ permission.
module Network.AWS.CloudWatchLogs.PutSubscriptionFilter
  ( -- * Creating a Request
    PutSubscriptionFilter (..),
    newPutSubscriptionFilter,

    -- * Request Lenses
    putSubscriptionFilter_roleArn,
    putSubscriptionFilter_distribution,
    putSubscriptionFilter_logGroupName,
    putSubscriptionFilter_filterName,
    putSubscriptionFilter_filterPattern,
    putSubscriptionFilter_destinationArn,

    -- * Destructuring the Response
    PutSubscriptionFilterResponse (..),
    newPutSubscriptionFilterResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutSubscriptionFilter' smart constructor.
data PutSubscriptionFilter = PutSubscriptionFilter'
  { -- | The ARN of an IAM role that grants CloudWatch Logs permissions to
    -- deliver ingested log events to the destination stream. You don\'t need
    -- to provide the ARN when you are working with a logical destination for
    -- cross-account delivery.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The method used to distribute log data to the destination. By default,
    -- log data is grouped by log stream, but the grouping can be set to random
    -- for a more even distribution. This property is only applicable when the
    -- destination is an Amazon Kinesis stream.
    distribution :: Prelude.Maybe Distribution,
    -- | The name of the log group.
    logGroupName :: Prelude.Text,
    -- | A name for the subscription filter. If you are updating an existing
    -- filter, you must specify the correct name in @filterName@. Otherwise,
    -- the call fails because you cannot associate a second filter with a log
    -- group. To find the name of the filter currently associated with a log
    -- group, use
    -- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeSubscriptionFilters.html DescribeSubscriptionFilters>.
    filterName :: Prelude.Text,
    -- | A filter pattern for subscribing to a filtered stream of log events.
    filterPattern :: Prelude.Text,
    -- | The ARN of the destination to deliver matching log events to. Currently,
    -- the supported destinations are:
    --
    -- -   An Amazon Kinesis stream belonging to the same account as the
    --     subscription filter, for same-account delivery.
    --
    -- -   A logical destination (specified using an ARN) belonging to a
    --     different account, for cross-account delivery.
    --
    -- -   An Amazon Kinesis Firehose delivery stream belonging to the same
    --     account as the subscription filter, for same-account delivery.
    --
    -- -   An AWS Lambda function belonging to the same account as the
    --     subscription filter, for same-account delivery.
    destinationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutSubscriptionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roleArn', 'putSubscriptionFilter_roleArn' - The ARN of an IAM role that grants CloudWatch Logs permissions to
-- deliver ingested log events to the destination stream. You don\'t need
-- to provide the ARN when you are working with a logical destination for
-- cross-account delivery.
--
-- 'distribution', 'putSubscriptionFilter_distribution' - The method used to distribute log data to the destination. By default,
-- log data is grouped by log stream, but the grouping can be set to random
-- for a more even distribution. This property is only applicable when the
-- destination is an Amazon Kinesis stream.
--
-- 'logGroupName', 'putSubscriptionFilter_logGroupName' - The name of the log group.
--
-- 'filterName', 'putSubscriptionFilter_filterName' - A name for the subscription filter. If you are updating an existing
-- filter, you must specify the correct name in @filterName@. Otherwise,
-- the call fails because you cannot associate a second filter with a log
-- group. To find the name of the filter currently associated with a log
-- group, use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeSubscriptionFilters.html DescribeSubscriptionFilters>.
--
-- 'filterPattern', 'putSubscriptionFilter_filterPattern' - A filter pattern for subscribing to a filtered stream of log events.
--
-- 'destinationArn', 'putSubscriptionFilter_destinationArn' - The ARN of the destination to deliver matching log events to. Currently,
-- the supported destinations are:
--
-- -   An Amazon Kinesis stream belonging to the same account as the
--     subscription filter, for same-account delivery.
--
-- -   A logical destination (specified using an ARN) belonging to a
--     different account, for cross-account delivery.
--
-- -   An Amazon Kinesis Firehose delivery stream belonging to the same
--     account as the subscription filter, for same-account delivery.
--
-- -   An AWS Lambda function belonging to the same account as the
--     subscription filter, for same-account delivery.
newPutSubscriptionFilter ::
  -- | 'logGroupName'
  Prelude.Text ->
  -- | 'filterName'
  Prelude.Text ->
  -- | 'filterPattern'
  Prelude.Text ->
  -- | 'destinationArn'
  Prelude.Text ->
  PutSubscriptionFilter
newPutSubscriptionFilter
  pLogGroupName_
  pFilterName_
  pFilterPattern_
  pDestinationArn_ =
    PutSubscriptionFilter'
      { roleArn = Prelude.Nothing,
        distribution = Prelude.Nothing,
        logGroupName = pLogGroupName_,
        filterName = pFilterName_,
        filterPattern = pFilterPattern_,
        destinationArn = pDestinationArn_
      }

-- | The ARN of an IAM role that grants CloudWatch Logs permissions to
-- deliver ingested log events to the destination stream. You don\'t need
-- to provide the ARN when you are working with a logical destination for
-- cross-account delivery.
putSubscriptionFilter_roleArn :: Lens.Lens' PutSubscriptionFilter (Prelude.Maybe Prelude.Text)
putSubscriptionFilter_roleArn = Lens.lens (\PutSubscriptionFilter' {roleArn} -> roleArn) (\s@PutSubscriptionFilter' {} a -> s {roleArn = a} :: PutSubscriptionFilter)

-- | The method used to distribute log data to the destination. By default,
-- log data is grouped by log stream, but the grouping can be set to random
-- for a more even distribution. This property is only applicable when the
-- destination is an Amazon Kinesis stream.
putSubscriptionFilter_distribution :: Lens.Lens' PutSubscriptionFilter (Prelude.Maybe Distribution)
putSubscriptionFilter_distribution = Lens.lens (\PutSubscriptionFilter' {distribution} -> distribution) (\s@PutSubscriptionFilter' {} a -> s {distribution = a} :: PutSubscriptionFilter)

-- | The name of the log group.
putSubscriptionFilter_logGroupName :: Lens.Lens' PutSubscriptionFilter Prelude.Text
putSubscriptionFilter_logGroupName = Lens.lens (\PutSubscriptionFilter' {logGroupName} -> logGroupName) (\s@PutSubscriptionFilter' {} a -> s {logGroupName = a} :: PutSubscriptionFilter)

-- | A name for the subscription filter. If you are updating an existing
-- filter, you must specify the correct name in @filterName@. Otherwise,
-- the call fails because you cannot associate a second filter with a log
-- group. To find the name of the filter currently associated with a log
-- group, use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeSubscriptionFilters.html DescribeSubscriptionFilters>.
putSubscriptionFilter_filterName :: Lens.Lens' PutSubscriptionFilter Prelude.Text
putSubscriptionFilter_filterName = Lens.lens (\PutSubscriptionFilter' {filterName} -> filterName) (\s@PutSubscriptionFilter' {} a -> s {filterName = a} :: PutSubscriptionFilter)

-- | A filter pattern for subscribing to a filtered stream of log events.
putSubscriptionFilter_filterPattern :: Lens.Lens' PutSubscriptionFilter Prelude.Text
putSubscriptionFilter_filterPattern = Lens.lens (\PutSubscriptionFilter' {filterPattern} -> filterPattern) (\s@PutSubscriptionFilter' {} a -> s {filterPattern = a} :: PutSubscriptionFilter)

-- | The ARN of the destination to deliver matching log events to. Currently,
-- the supported destinations are:
--
-- -   An Amazon Kinesis stream belonging to the same account as the
--     subscription filter, for same-account delivery.
--
-- -   A logical destination (specified using an ARN) belonging to a
--     different account, for cross-account delivery.
--
-- -   An Amazon Kinesis Firehose delivery stream belonging to the same
--     account as the subscription filter, for same-account delivery.
--
-- -   An AWS Lambda function belonging to the same account as the
--     subscription filter, for same-account delivery.
putSubscriptionFilter_destinationArn :: Lens.Lens' PutSubscriptionFilter Prelude.Text
putSubscriptionFilter_destinationArn = Lens.lens (\PutSubscriptionFilter' {destinationArn} -> destinationArn) (\s@PutSubscriptionFilter' {} a -> s {destinationArn = a} :: PutSubscriptionFilter)

instance Prelude.AWSRequest PutSubscriptionFilter where
  type
    Rs PutSubscriptionFilter =
      PutSubscriptionFilterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutSubscriptionFilterResponse'

instance Prelude.Hashable PutSubscriptionFilter

instance Prelude.NFData PutSubscriptionFilter

instance Prelude.ToHeaders PutSubscriptionFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Logs_20140328.PutSubscriptionFilter" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutSubscriptionFilter where
  toJSON PutSubscriptionFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("roleArn" Prelude..=) Prelude.<$> roleArn,
            ("distribution" Prelude..=) Prelude.<$> distribution,
            Prelude.Just
              ("logGroupName" Prelude..= logGroupName),
            Prelude.Just ("filterName" Prelude..= filterName),
            Prelude.Just
              ("filterPattern" Prelude..= filterPattern),
            Prelude.Just
              ("destinationArn" Prelude..= destinationArn)
          ]
      )

instance Prelude.ToPath PutSubscriptionFilter where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutSubscriptionFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSubscriptionFilterResponse' smart constructor.
data PutSubscriptionFilterResponse = PutSubscriptionFilterResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutSubscriptionFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutSubscriptionFilterResponse ::
  PutSubscriptionFilterResponse
newPutSubscriptionFilterResponse =
  PutSubscriptionFilterResponse'

instance Prelude.NFData PutSubscriptionFilterResponse
