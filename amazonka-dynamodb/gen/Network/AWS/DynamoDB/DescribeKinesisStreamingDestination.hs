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
-- Module      : Network.AWS.DynamoDB.DescribeKinesisStreamingDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the status of Kinesis streaming.
module Network.AWS.DynamoDB.DescribeKinesisStreamingDestination
  ( -- * Creating a Request
    DescribeKinesisStreamingDestination (..),
    newDescribeKinesisStreamingDestination,

    -- * Request Lenses
    describeKinesisStreamingDestination_tableName,

    -- * Destructuring the Response
    DescribeKinesisStreamingDestinationResponse (..),
    newDescribeKinesisStreamingDestinationResponse,

    -- * Response Lenses
    describeKinesisStreamingDestinationResponse_tableName,
    describeKinesisStreamingDestinationResponse_kinesisDataStreamDestinations,
    describeKinesisStreamingDestinationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeKinesisStreamingDestination' smart constructor.
data DescribeKinesisStreamingDestination = DescribeKinesisStreamingDestination'
  { -- | The name of the table being described.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeKinesisStreamingDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'describeKinesisStreamingDestination_tableName' - The name of the table being described.
newDescribeKinesisStreamingDestination ::
  -- | 'tableName'
  Core.Text ->
  DescribeKinesisStreamingDestination
newDescribeKinesisStreamingDestination pTableName_ =
  DescribeKinesisStreamingDestination'
    { tableName =
        pTableName_
    }

-- | The name of the table being described.
describeKinesisStreamingDestination_tableName :: Lens.Lens' DescribeKinesisStreamingDestination Core.Text
describeKinesisStreamingDestination_tableName = Lens.lens (\DescribeKinesisStreamingDestination' {tableName} -> tableName) (\s@DescribeKinesisStreamingDestination' {} a -> s {tableName = a} :: DescribeKinesisStreamingDestination)

instance
  Core.AWSRequest
    DescribeKinesisStreamingDestination
  where
  type
    AWSResponse DescribeKinesisStreamingDestination =
      DescribeKinesisStreamingDestinationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeKinesisStreamingDestinationResponse'
            Core.<$> (x Core..?> "TableName")
            Core.<*> ( x Core..?> "KinesisDataStreamDestinations"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeKinesisStreamingDestination

instance
  Core.NFData
    DescribeKinesisStreamingDestination

instance
  Core.ToHeaders
    DescribeKinesisStreamingDestination
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.DescribeKinesisStreamingDestination" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeKinesisStreamingDestination
  where
  toJSON DescribeKinesisStreamingDestination' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TableName" Core..= tableName)]
      )

instance
  Core.ToPath
    DescribeKinesisStreamingDestination
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeKinesisStreamingDestination
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeKinesisStreamingDestinationResponse' smart constructor.
data DescribeKinesisStreamingDestinationResponse = DescribeKinesisStreamingDestinationResponse'
  { -- | The name of the table being described.
    tableName :: Core.Maybe Core.Text,
    -- | The list of replica structures for the table being described.
    kinesisDataStreamDestinations :: Core.Maybe [KinesisDataStreamDestination],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeKinesisStreamingDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'describeKinesisStreamingDestinationResponse_tableName' - The name of the table being described.
--
-- 'kinesisDataStreamDestinations', 'describeKinesisStreamingDestinationResponse_kinesisDataStreamDestinations' - The list of replica structures for the table being described.
--
-- 'httpStatus', 'describeKinesisStreamingDestinationResponse_httpStatus' - The response's http status code.
newDescribeKinesisStreamingDestinationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeKinesisStreamingDestinationResponse
newDescribeKinesisStreamingDestinationResponse
  pHttpStatus_ =
    DescribeKinesisStreamingDestinationResponse'
      { tableName =
          Core.Nothing,
        kinesisDataStreamDestinations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The name of the table being described.
describeKinesisStreamingDestinationResponse_tableName :: Lens.Lens' DescribeKinesisStreamingDestinationResponse (Core.Maybe Core.Text)
describeKinesisStreamingDestinationResponse_tableName = Lens.lens (\DescribeKinesisStreamingDestinationResponse' {tableName} -> tableName) (\s@DescribeKinesisStreamingDestinationResponse' {} a -> s {tableName = a} :: DescribeKinesisStreamingDestinationResponse)

-- | The list of replica structures for the table being described.
describeKinesisStreamingDestinationResponse_kinesisDataStreamDestinations :: Lens.Lens' DescribeKinesisStreamingDestinationResponse (Core.Maybe [KinesisDataStreamDestination])
describeKinesisStreamingDestinationResponse_kinesisDataStreamDestinations = Lens.lens (\DescribeKinesisStreamingDestinationResponse' {kinesisDataStreamDestinations} -> kinesisDataStreamDestinations) (\s@DescribeKinesisStreamingDestinationResponse' {} a -> s {kinesisDataStreamDestinations = a} :: DescribeKinesisStreamingDestinationResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeKinesisStreamingDestinationResponse_httpStatus :: Lens.Lens' DescribeKinesisStreamingDestinationResponse Core.Int
describeKinesisStreamingDestinationResponse_httpStatus = Lens.lens (\DescribeKinesisStreamingDestinationResponse' {httpStatus} -> httpStatus) (\s@DescribeKinesisStreamingDestinationResponse' {} a -> s {httpStatus = a} :: DescribeKinesisStreamingDestinationResponse)

instance
  Core.NFData
    DescribeKinesisStreamingDestinationResponse
