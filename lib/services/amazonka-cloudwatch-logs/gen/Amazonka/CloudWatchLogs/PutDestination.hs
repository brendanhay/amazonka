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
-- Module      : Amazonka.CloudWatchLogs.PutDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CloudWatchLogs.PutDestination
  ( -- * Creating a Request
    PutDestination (..),
    newPutDestination,

    -- * Request Lenses
    putDestination_tags,
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

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutDestination' smart constructor.
data PutDestination = PutDestination'
  { -- | An optional list of key-value pairs to associate with the resource.
    --
    -- For more information about tagging, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A name for the destination.
    destinationName :: Prelude.Text,
    -- | The ARN of an Amazon Kinesis stream to which to deliver matching log
    -- events.
    targetArn :: Prelude.Text,
    -- | The ARN of an IAM role that grants CloudWatch Logs permissions to call
    -- the Amazon Kinesis @PutRecord@ operation on the destination stream.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putDestination_tags' - An optional list of key-value pairs to associate with the resource.
--
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
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
  Prelude.Text ->
  -- | 'targetArn'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  PutDestination
newPutDestination
  pDestinationName_
  pTargetArn_
  pRoleArn_ =
    PutDestination'
      { tags = Prelude.Nothing,
        destinationName = pDestinationName_,
        targetArn = pTargetArn_,
        roleArn = pRoleArn_
      }

-- | An optional list of key-value pairs to associate with the resource.
--
-- For more information about tagging, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
putDestination_tags :: Lens.Lens' PutDestination (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putDestination_tags = Lens.lens (\PutDestination' {tags} -> tags) (\s@PutDestination' {} a -> s {tags = a} :: PutDestination) Prelude.. Lens.mapping Lens.coerced

-- | A name for the destination.
putDestination_destinationName :: Lens.Lens' PutDestination Prelude.Text
putDestination_destinationName = Lens.lens (\PutDestination' {destinationName} -> destinationName) (\s@PutDestination' {} a -> s {destinationName = a} :: PutDestination)

-- | The ARN of an Amazon Kinesis stream to which to deliver matching log
-- events.
putDestination_targetArn :: Lens.Lens' PutDestination Prelude.Text
putDestination_targetArn = Lens.lens (\PutDestination' {targetArn} -> targetArn) (\s@PutDestination' {} a -> s {targetArn = a} :: PutDestination)

-- | The ARN of an IAM role that grants CloudWatch Logs permissions to call
-- the Amazon Kinesis @PutRecord@ operation on the destination stream.
putDestination_roleArn :: Lens.Lens' PutDestination Prelude.Text
putDestination_roleArn = Lens.lens (\PutDestination' {roleArn} -> roleArn) (\s@PutDestination' {} a -> s {roleArn = a} :: PutDestination)

instance Core.AWSRequest PutDestination where
  type
    AWSResponse PutDestination =
      PutDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutDestinationResponse'
            Prelude.<$> (x Data..?> "destination")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutDestination where
  hashWithSalt _salt PutDestination' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData PutDestination where
  rnf PutDestination' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf targetArn
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders PutDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.PutDestination" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDestination where
  toJSON PutDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("destinationName" Data..= destinationName),
            Prelude.Just ("targetArn" Data..= targetArn),
            Prelude.Just ("roleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath PutDestination where
  toPath = Prelude.const "/"

instance Data.ToQuery PutDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDestinationResponse' smart constructor.
data PutDestinationResponse = PutDestinationResponse'
  { -- | The destination.
    destination :: Prelude.Maybe Destination,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutDestinationResponse
newPutDestinationResponse pHttpStatus_ =
  PutDestinationResponse'
    { destination =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The destination.
putDestinationResponse_destination :: Lens.Lens' PutDestinationResponse (Prelude.Maybe Destination)
putDestinationResponse_destination = Lens.lens (\PutDestinationResponse' {destination} -> destination) (\s@PutDestinationResponse' {} a -> s {destination = a} :: PutDestinationResponse)

-- | The response's http status code.
putDestinationResponse_httpStatus :: Lens.Lens' PutDestinationResponse Prelude.Int
putDestinationResponse_httpStatus = Lens.lens (\PutDestinationResponse' {httpStatus} -> httpStatus) (\s@PutDestinationResponse' {} a -> s {httpStatus = a} :: PutDestinationResponse)

instance Prelude.NFData PutDestinationResponse where
  rnf PutDestinationResponse' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf httpStatus
