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
-- Module      : Amazonka.DynamoDB.DescribeKinesisStreamingDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the status of Kinesis streaming.
module Amazonka.DynamoDB.DescribeKinesisStreamingDestination
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeKinesisStreamingDestination' smart constructor.
data DescribeKinesisStreamingDestination = DescribeKinesisStreamingDestination'
  { -- | The name of the table being described.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeKinesisStreamingDestination
newDescribeKinesisStreamingDestination pTableName_ =
  DescribeKinesisStreamingDestination'
    { tableName =
        pTableName_
    }

-- | The name of the table being described.
describeKinesisStreamingDestination_tableName :: Lens.Lens' DescribeKinesisStreamingDestination Prelude.Text
describeKinesisStreamingDestination_tableName = Lens.lens (\DescribeKinesisStreamingDestination' {tableName} -> tableName) (\s@DescribeKinesisStreamingDestination' {} a -> s {tableName = a} :: DescribeKinesisStreamingDestination)

instance
  Core.AWSRequest
    DescribeKinesisStreamingDestination
  where
  type
    AWSResponse DescribeKinesisStreamingDestination =
      DescribeKinesisStreamingDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeKinesisStreamingDestinationResponse'
            Prelude.<$> (x Data..?> "TableName")
              Prelude.<*> ( x Data..?> "KinesisDataStreamDestinations"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeKinesisStreamingDestination
  where
  hashWithSalt
    _salt
    DescribeKinesisStreamingDestination' {..} =
      _salt `Prelude.hashWithSalt` tableName

instance
  Prelude.NFData
    DescribeKinesisStreamingDestination
  where
  rnf DescribeKinesisStreamingDestination' {..} =
    Prelude.rnf tableName

instance
  Data.ToHeaders
    DescribeKinesisStreamingDestination
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeKinesisStreamingDestination" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeKinesisStreamingDestination
  where
  toJSON DescribeKinesisStreamingDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TableName" Data..= tableName)]
      )

instance
  Data.ToPath
    DescribeKinesisStreamingDestination
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeKinesisStreamingDestination
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeKinesisStreamingDestinationResponse' smart constructor.
data DescribeKinesisStreamingDestinationResponse = DescribeKinesisStreamingDestinationResponse'
  { -- | The name of the table being described.
    tableName :: Prelude.Maybe Prelude.Text,
    -- | The list of replica structures for the table being described.
    kinesisDataStreamDestinations :: Prelude.Maybe [KinesisDataStreamDestination],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeKinesisStreamingDestinationResponse
newDescribeKinesisStreamingDestinationResponse
  pHttpStatus_ =
    DescribeKinesisStreamingDestinationResponse'
      { tableName =
          Prelude.Nothing,
        kinesisDataStreamDestinations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The name of the table being described.
describeKinesisStreamingDestinationResponse_tableName :: Lens.Lens' DescribeKinesisStreamingDestinationResponse (Prelude.Maybe Prelude.Text)
describeKinesisStreamingDestinationResponse_tableName = Lens.lens (\DescribeKinesisStreamingDestinationResponse' {tableName} -> tableName) (\s@DescribeKinesisStreamingDestinationResponse' {} a -> s {tableName = a} :: DescribeKinesisStreamingDestinationResponse)

-- | The list of replica structures for the table being described.
describeKinesisStreamingDestinationResponse_kinesisDataStreamDestinations :: Lens.Lens' DescribeKinesisStreamingDestinationResponse (Prelude.Maybe [KinesisDataStreamDestination])
describeKinesisStreamingDestinationResponse_kinesisDataStreamDestinations = Lens.lens (\DescribeKinesisStreamingDestinationResponse' {kinesisDataStreamDestinations} -> kinesisDataStreamDestinations) (\s@DescribeKinesisStreamingDestinationResponse' {} a -> s {kinesisDataStreamDestinations = a} :: DescribeKinesisStreamingDestinationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeKinesisStreamingDestinationResponse_httpStatus :: Lens.Lens' DescribeKinesisStreamingDestinationResponse Prelude.Int
describeKinesisStreamingDestinationResponse_httpStatus = Lens.lens (\DescribeKinesisStreamingDestinationResponse' {httpStatus} -> httpStatus) (\s@DescribeKinesisStreamingDestinationResponse' {} a -> s {httpStatus = a} :: DescribeKinesisStreamingDestinationResponse)

instance
  Prelude.NFData
    DescribeKinesisStreamingDestinationResponse
  where
  rnf DescribeKinesisStreamingDestinationResponse' {..} =
    Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf kinesisDataStreamDestinations
      `Prelude.seq` Prelude.rnf httpStatus
