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
-- Module      : Amazonka.Redshift.DescribeDataSharesForProducer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of datashares when the account identifier being called is
-- a producer account identifier.
--
-- This operation returns paginated results.
module Amazonka.Redshift.DescribeDataSharesForProducer
  ( -- * Creating a Request
    DescribeDataSharesForProducer (..),
    newDescribeDataSharesForProducer,

    -- * Request Lenses
    describeDataSharesForProducer_marker,
    describeDataSharesForProducer_maxRecords,
    describeDataSharesForProducer_producerArn,
    describeDataSharesForProducer_status,

    -- * Destructuring the Response
    DescribeDataSharesForProducerResponse (..),
    newDescribeDataSharesForProducerResponse,

    -- * Response Lenses
    describeDataSharesForProducerResponse_dataShares,
    describeDataSharesForProducerResponse_marker,
    describeDataSharesForProducerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDataSharesForProducer' smart constructor.
data DescribeDataSharesForProducer = DescribeDataSharesForProducer'
  { -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeDataSharesForProducer
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
    -- | The Amazon Resource Name (ARN) of the producer that returns in the list
    -- of datashares.
    producerArn :: Prelude.Maybe Prelude.Text,
    -- | An identifier giving the status of a datashare in the producer. If this
    -- field is specified, Amazon Redshift returns the list of datashares that
    -- have the specified status.
    status :: Prelude.Maybe DataShareStatusForProducer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDataSharesForProducer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'describeDataSharesForProducer_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForProducer
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'maxRecords', 'describeDataSharesForProducer_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- 'producerArn', 'describeDataSharesForProducer_producerArn' - The Amazon Resource Name (ARN) of the producer that returns in the list
-- of datashares.
--
-- 'status', 'describeDataSharesForProducer_status' - An identifier giving the status of a datashare in the producer. If this
-- field is specified, Amazon Redshift returns the list of datashares that
-- have the specified status.
newDescribeDataSharesForProducer ::
  DescribeDataSharesForProducer
newDescribeDataSharesForProducer =
  DescribeDataSharesForProducer'
    { marker =
        Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      producerArn = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForProducer
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeDataSharesForProducer_marker :: Lens.Lens' DescribeDataSharesForProducer (Prelude.Maybe Prelude.Text)
describeDataSharesForProducer_marker = Lens.lens (\DescribeDataSharesForProducer' {marker} -> marker) (\s@DescribeDataSharesForProducer' {} a -> s {marker = a} :: DescribeDataSharesForProducer)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
describeDataSharesForProducer_maxRecords :: Lens.Lens' DescribeDataSharesForProducer (Prelude.Maybe Prelude.Int)
describeDataSharesForProducer_maxRecords = Lens.lens (\DescribeDataSharesForProducer' {maxRecords} -> maxRecords) (\s@DescribeDataSharesForProducer' {} a -> s {maxRecords = a} :: DescribeDataSharesForProducer)

-- | The Amazon Resource Name (ARN) of the producer that returns in the list
-- of datashares.
describeDataSharesForProducer_producerArn :: Lens.Lens' DescribeDataSharesForProducer (Prelude.Maybe Prelude.Text)
describeDataSharesForProducer_producerArn = Lens.lens (\DescribeDataSharesForProducer' {producerArn} -> producerArn) (\s@DescribeDataSharesForProducer' {} a -> s {producerArn = a} :: DescribeDataSharesForProducer)

-- | An identifier giving the status of a datashare in the producer. If this
-- field is specified, Amazon Redshift returns the list of datashares that
-- have the specified status.
describeDataSharesForProducer_status :: Lens.Lens' DescribeDataSharesForProducer (Prelude.Maybe DataShareStatusForProducer)
describeDataSharesForProducer_status = Lens.lens (\DescribeDataSharesForProducer' {status} -> status) (\s@DescribeDataSharesForProducer' {} a -> s {status = a} :: DescribeDataSharesForProducer)

instance Core.AWSPager DescribeDataSharesForProducer where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDataSharesForProducerResponse_marker
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDataSharesForProducerResponse_dataShares
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeDataSharesForProducer_marker
          Lens..~ rs
          Lens.^? describeDataSharesForProducerResponse_marker
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeDataSharesForProducer
  where
  type
    AWSResponse DescribeDataSharesForProducer =
      DescribeDataSharesForProducerResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeDataSharesForProducerResult"
      ( \s h x ->
          DescribeDataSharesForProducerResponse'
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
    DescribeDataSharesForProducer
  where
  hashWithSalt _salt DescribeDataSharesForProducer' {..} =
    _salt
      `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` maxRecords
      `Prelude.hashWithSalt` producerArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData DescribeDataSharesForProducer where
  rnf DescribeDataSharesForProducer' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf maxRecords
      `Prelude.seq` Prelude.rnf producerArn
      `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders DescribeDataSharesForProducer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDataSharesForProducer where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDataSharesForProducer where
  toQuery DescribeDataSharesForProducer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeDataSharesForProducer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Marker" Data.=: marker,
        "MaxRecords" Data.=: maxRecords,
        "ProducerArn" Data.=: producerArn,
        "Status" Data.=: status
      ]

-- | /See:/ 'newDescribeDataSharesForProducerResponse' smart constructor.
data DescribeDataSharesForProducerResponse = DescribeDataSharesForProducerResponse'
  { -- | Shows the results of datashares available for producers.
    dataShares :: Prelude.Maybe [DataShare],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeDataSharesForProducer
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
-- Create a value of 'DescribeDataSharesForProducerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataShares', 'describeDataSharesForProducerResponse_dataShares' - Shows the results of datashares available for producers.
--
-- 'marker', 'describeDataSharesForProducerResponse_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForProducer
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
--
-- 'httpStatus', 'describeDataSharesForProducerResponse_httpStatus' - The response's http status code.
newDescribeDataSharesForProducerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDataSharesForProducerResponse
newDescribeDataSharesForProducerResponse pHttpStatus_ =
  DescribeDataSharesForProducerResponse'
    { dataShares =
        Prelude.Nothing,
      marker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Shows the results of datashares available for producers.
describeDataSharesForProducerResponse_dataShares :: Lens.Lens' DescribeDataSharesForProducerResponse (Prelude.Maybe [DataShare])
describeDataSharesForProducerResponse_dataShares = Lens.lens (\DescribeDataSharesForProducerResponse' {dataShares} -> dataShares) (\s@DescribeDataSharesForProducerResponse' {} a -> s {dataShares = a} :: DescribeDataSharesForProducerResponse) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForProducer
-- request exceed the value specified in @MaxRecords@, Amazon Web Services
-- returns a value in the @Marker@ field of the response. You can retrieve
-- the next set of response records by providing the returned marker value
-- in the @Marker@ parameter and retrying the request.
describeDataSharesForProducerResponse_marker :: Lens.Lens' DescribeDataSharesForProducerResponse (Prelude.Maybe Prelude.Text)
describeDataSharesForProducerResponse_marker = Lens.lens (\DescribeDataSharesForProducerResponse' {marker} -> marker) (\s@DescribeDataSharesForProducerResponse' {} a -> s {marker = a} :: DescribeDataSharesForProducerResponse)

-- | The response's http status code.
describeDataSharesForProducerResponse_httpStatus :: Lens.Lens' DescribeDataSharesForProducerResponse Prelude.Int
describeDataSharesForProducerResponse_httpStatus = Lens.lens (\DescribeDataSharesForProducerResponse' {httpStatus} -> httpStatus) (\s@DescribeDataSharesForProducerResponse' {} a -> s {httpStatus = a} :: DescribeDataSharesForProducerResponse)

instance
  Prelude.NFData
    DescribeDataSharesForProducerResponse
  where
  rnf DescribeDataSharesForProducerResponse' {..} =
    Prelude.rnf dataShares
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf httpStatus
