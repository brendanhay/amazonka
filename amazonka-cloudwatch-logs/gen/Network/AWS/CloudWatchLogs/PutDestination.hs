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
-- Module      : Network.AWS.CloudWatchLogs.PutDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates a destination. This operation is used only to create
-- destinations for cross-account subscriptions.
--
-- A destination encapsulates a physical resource (such as an Amazon
-- Kinesis stream) and enables you to subscribe to a real-time stream of
-- log events for a different account, ingested using
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutLogEvents.html PutLogEvents>.
--
-- Through an access policy, a destination controls what is written to it.
-- By default, @PutDestination@ does not set any access policy with the
-- destination, which means a cross-account user cannot call
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutSubscriptionFilter.html PutSubscriptionFilter>
-- against this destination. To enable this, the destination owner must
-- call
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_PutDestinationPolicy.html PutDestinationPolicy>
-- after @PutDestination@.
--
-- To perform a @PutDestination@ operation, you must also have the
-- @iam:PassRole@ permission.
module Network.AWS.CloudWatchLogs.PutDestination
  ( -- * Creating a Request
    PutDestination (..),
    newPutDestination,

    -- * Request Lenses
    putDestination_destinationName,
    putDestination_targetArn,
    putDestination_roleArn,

    -- * Destructuring the Response
    PutDestinationResponse (..),
    newPutDestinationResponse,

    -- * Response Lenses
    putDestinationResponse_destination,
    putDestinationResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutDestination' smart constructor.
data PutDestination = PutDestination'
  { -- | A name for the destination.
    destinationName :: Core.Text,
    -- | The ARN of an Amazon Kinesis stream to which to deliver matching log
    -- events.
    targetArn :: Core.Text,
    -- | The ARN of an IAM role that grants CloudWatch Logs permissions to call
    -- the Amazon Kinesis @PutRecord@ operation on the destination stream.
    roleArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationName', 'putDestination_destinationName' - A name for the destination.
--
-- 'targetArn', 'putDestination_targetArn' - The ARN of an Amazon Kinesis stream to which to deliver matching log
-- events.
--
-- 'roleArn', 'putDestination_roleArn' - The ARN of an IAM role that grants CloudWatch Logs permissions to call
-- the Amazon Kinesis @PutRecord@ operation on the destination stream.
newPutDestination ::
  -- | 'destinationName'
  Core.Text ->
  -- | 'targetArn'
  Core.Text ->
  -- | 'roleArn'
  Core.Text ->
  PutDestination
newPutDestination
  pDestinationName_
  pTargetArn_
  pRoleArn_ =
    PutDestination'
      { destinationName =
          pDestinationName_,
        targetArn = pTargetArn_,
        roleArn = pRoleArn_
      }

-- | A name for the destination.
putDestination_destinationName :: Lens.Lens' PutDestination Core.Text
putDestination_destinationName = Lens.lens (\PutDestination' {destinationName} -> destinationName) (\s@PutDestination' {} a -> s {destinationName = a} :: PutDestination)

-- | The ARN of an Amazon Kinesis stream to which to deliver matching log
-- events.
putDestination_targetArn :: Lens.Lens' PutDestination Core.Text
putDestination_targetArn = Lens.lens (\PutDestination' {targetArn} -> targetArn) (\s@PutDestination' {} a -> s {targetArn = a} :: PutDestination)

-- | The ARN of an IAM role that grants CloudWatch Logs permissions to call
-- the Amazon Kinesis @PutRecord@ operation on the destination stream.
putDestination_roleArn :: Lens.Lens' PutDestination Core.Text
putDestination_roleArn = Lens.lens (\PutDestination' {roleArn} -> roleArn) (\s@PutDestination' {} a -> s {roleArn = a} :: PutDestination)

instance Core.AWSRequest PutDestination where
  type
    AWSResponse PutDestination =
      PutDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutDestinationResponse'
            Core.<$> (x Core..?> "destination")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutDestination

instance Core.NFData PutDestination

instance Core.ToHeaders PutDestination where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.PutDestination" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutDestination where
  toJSON PutDestination' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("destinationName" Core..= destinationName),
            Core.Just ("targetArn" Core..= targetArn),
            Core.Just ("roleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath PutDestination where
  toPath = Core.const "/"

instance Core.ToQuery PutDestination where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutDestinationResponse' smart constructor.
data PutDestinationResponse = PutDestinationResponse'
  { -- | The destination.
    destination :: Core.Maybe Destination,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'putDestinationResponse_destination' - The destination.
--
-- 'httpStatus', 'putDestinationResponse_httpStatus' - The response's http status code.
newPutDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutDestinationResponse
newPutDestinationResponse pHttpStatus_ =
  PutDestinationResponse'
    { destination = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The destination.
putDestinationResponse_destination :: Lens.Lens' PutDestinationResponse (Core.Maybe Destination)
putDestinationResponse_destination = Lens.lens (\PutDestinationResponse' {destination} -> destination) (\s@PutDestinationResponse' {} a -> s {destination = a} :: PutDestinationResponse)

-- | The response's http status code.
putDestinationResponse_httpStatus :: Lens.Lens' PutDestinationResponse Core.Int
putDestinationResponse_httpStatus = Lens.lens (\PutDestinationResponse' {httpStatus} -> httpStatus) (\s@PutDestinationResponse' {} a -> s {httpStatus = a} :: PutDestinationResponse)

instance Core.NFData PutDestinationResponse
