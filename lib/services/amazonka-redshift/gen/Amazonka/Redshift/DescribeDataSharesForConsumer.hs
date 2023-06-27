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
-- Module      : Amazonka.Redshift.DescribeDataSharesForConsumer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of datashares where the account identifier being called
-- is a consumer account identifier.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeDataSharesForConsumer
  ( -- * Creating a Request
    DescribeDataSharesForConsumer (..),
    newDescribeDataSharesForConsumer,

    -- * Request Lenses
    describeDataSharesForConsumer_consumerArn,
    describeDataSharesForConsumer_marker,
    describeDataSharesForConsumer_maxRecords,
    describeDataSharesForConsumer_status,

    -- * Destructuring the Response
    DescribeDataSharesForConsumerResponse (..),
    newDescribeDataSharesForConsumerResponse,

    -- * Response Lenses
    describeDataSharesForConsumerResponse_dataShares,
    describeDataSharesForConsumerResponse_marker,
    describeDataSharesForConsumerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataSharesForConsumer' smart constructor.
data DescribeDataSharesForConsumer = DescribeDataSharesForConsumer'
  { -- | The Amazon Resource Name (ARN) of the consumer that returns in the list
    -- of datashares.
    consumerArn :: Prelude.Maybe Prelude.Text,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeDataSharesForConsumer
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | An identifier giving the status of a datashare in the consumer cluster.
    -- If this field is specified, Amazon Redshift returns the list of
    -- datashares that have the specified status.
    status :: Prelude.Maybe DataShareStatusForConsumer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSharesForConsumer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'consumerArn', 'describeDataSharesForConsumer_consumerArn' - The Amazon Resource Name (ARN) of the consumer that returns in the list
-- of datashares.
--
-- 'marker', 'describeDataSharesForConsumer_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForConsumer
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'maxRecords', 'describeDataSharesForConsumer_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- 'status', 'describeDataSharesForConsumer_status' - An identifier giving the status of a datashare in the consumer cluster.
-- If this field is specified, Amazon Redshift returns the list of
-- datashares that have the specified status.
newDescribeDataSharesForConsumer ::
  DescribeDataSharesForConsumer
newDescribeDataSharesForConsumer =
  DescribeDataSharesForConsumer'
    { consumerArn =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the consumer that returns in the list
-- of datashares.
describeDataSharesForConsumer_consumerArn :: Lens.Lens' DescribeDataSharesForConsumer (Prelude.Maybe Prelude.Text)
describeDataSharesForConsumer_consumerArn = Lens.lens (\DescribeDataSharesForConsumer' {consumerArn} -> consumerArn) (\s@DescribeDataSharesForConsumer' {} a -> s {consumerArn = a} :: DescribeDataSharesForConsumer)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForConsumer
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeDataSharesForConsumer_marker :: Lens.Lens' DescribeDataSharesForConsumer (Prelude.Maybe Prelude.Text)
describeDataSharesForConsumer_marker = Lens.lens (\DescribeDataSharesForConsumer' {marker} -> marker) (\s@DescribeDataSharesForConsumer' {} a -> s {marker = a} :: DescribeDataSharesForConsumer)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
describeDataSharesForConsumer_maxRecords :: Lens.Lens' DescribeDataSharesForConsumer (Prelude.Maybe Prelude.Int)
describeDataSharesForConsumer_maxRecords = Lens.lens (\DescribeDataSharesForConsumer' {maxRecords} -> maxRecords) (\s@DescribeDataSharesForConsumer' {} a -> s {maxRecords = a} :: DescribeDataSharesForConsumer)

-- | An identifier giving the status of a datashare in the consumer cluster.
-- If this field is specified, Amazon Redshift returns the list of
-- datashares that have the specified status.
describeDataSharesForConsumer_status :: Lens.Lens' DescribeDataSharesForConsumer (Prelude.Maybe DataShareStatusForConsumer)
describeDataSharesForConsumer_status = Lens.lens (\DescribeDataSharesForConsumer' {status} -> status) (\s@DescribeDataSharesForConsumer' {} a -> s {status = a} :: DescribeDataSharesForConsumer)

instance Core.AWSPager DescribeDataSharesForConsumer where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDataSharesForConsumerResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDataSharesForConsumerResponse_dataShares
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeDataSharesForConsumer_marker
          Lens..~ rs
          Lens.^? describeDataSharesForConsumerResponse_marker
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeDataSharesForConsumer
  where
  type
    AWSResponse DescribeDataSharesForConsumer =
      DescribeDataSharesForConsumerResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDataSharesForConsumerResult"
      ( \s h x ->
          DescribeDataSharesForConsumerResponse'
            Prelude.<$> ( x
                            Data..@? "DataShares"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDataSharesForConsumer
  where
  hashWithSalt _salt DescribeDataSharesForConsumer' {..} =
    _salt
      `Prelude.hashWithSalt` consumerArn
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` status

instance Prelude.NFData DescribeDataSharesForConsumer where
  rnf DescribeDataSharesForConsumer' {..} =
    Prelude.rnf consumerArn
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders DescribeDataSharesForConsumer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDataSharesForConsumer where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDataSharesForConsumer where
  toQuery DescribeDataSharesForConsumer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDataSharesForConsumer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "ConsumerArn" Data.=: consumerArn,
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "Status" Data.=: status
      ]

-- | /See:/ 'newDescribeDataSharesForConsumerResponse' smart constructor.
data DescribeDataSharesForConsumerResponse = DescribeDataSharesForConsumerResponse'
  { -- | Shows the results of datashares available for consumers.
    dataShares :: Prelude.Maybe [DataShare],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeDataSharesForConsumer
    -- request exceed the value specified in @MaxRecords@, Amazon Web Services
    -- returns a value in the @Marker@ field of the response. You can retrieve
    -- the next set of response records by providing the returned marker value
    -- in the @Marker@ parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSharesForConsumerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataShares', 'describeDataSharesForConsumerResponse_dataShares' - Shows the results of datashares available for consumers.
--
-- 'marker', 'describeDataSharesForConsumerResponse_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForConsumer
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'httpStatus', 'describeDataSharesForConsumerResponse_httpStatus' - The response's http status code.
newDescribeDataSharesForConsumerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDataSharesForConsumerResponse
newDescribeDataSharesForConsumerResponse pHttpStatus_ =
  DescribeDataSharesForConsumerResponse'
    { dataShares =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Shows the results of datashares available for consumers.
describeDataSharesForConsumerResponse_dataShares :: Lens.Lens' DescribeDataSharesForConsumerResponse (Prelude.Maybe [DataShare])
describeDataSharesForConsumerResponse_dataShares = Lens.lens (\DescribeDataSharesForConsumerResponse' {dataShares} -> dataShares) (\s@DescribeDataSharesForConsumerResponse' {} a -> s {dataShares = a} :: DescribeDataSharesForConsumerResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForConsumer
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeDataSharesForConsumerResponse_marker :: Lens.Lens' DescribeDataSharesForConsumerResponse (Prelude.Maybe Prelude.Text)
describeDataSharesForConsumerResponse_marker = Lens.lens (\DescribeDataSharesForConsumerResponse' {marker} -> marker) (\s@DescribeDataSharesForConsumerResponse' {} a -> s {marker = a} :: DescribeDataSharesForConsumerResponse)

-- | The response's http status code.
describeDataSharesForConsumerResponse_httpStatus :: Lens.Lens' DescribeDataSharesForConsumerResponse Prelude.Int
describeDataSharesForConsumerResponse_httpStatus = Lens.lens (\DescribeDataSharesForConsumerResponse' {httpStatus} -> httpStatus) (\s@DescribeDataSharesForConsumerResponse' {} a -> s {httpStatus = a} :: DescribeDataSharesForConsumerResponse)

instance
  Prelude.NFData
    DescribeDataSharesForConsumerResponse
  where
  rnf DescribeDataSharesForConsumerResponse' {..} =
    Prelude.rnf dataShares
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
