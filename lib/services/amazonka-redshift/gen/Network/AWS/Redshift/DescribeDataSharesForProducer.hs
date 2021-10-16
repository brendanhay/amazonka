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
-- Module      : Network.AWS.Redshift.DescribeDataSharesForProducer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of datashares when the account identifier being called is
-- a producer account identifier.
module Network.AWS.Redshift.DescribeDataSharesForProducer
  ( -- * Creating a Request
    DescribeDataSharesForProducer (..),
    newDescribeDataSharesForProducer,

    -- * Request Lenses
    describeDataSharesForProducer_status,
    describeDataSharesForProducer_producerArn,
    describeDataSharesForProducer_maxRecords,
    describeDataSharesForProducer_marker,

    -- * Destructuring the Response
    DescribeDataSharesForProducerResponse (..),
    newDescribeDataSharesForProducerResponse,

    -- * Response Lenses
    describeDataSharesForProducerResponse_dataShares,
    describeDataSharesForProducerResponse_marker,
    describeDataSharesForProducerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDataSharesForProducer' smart constructor.
data DescribeDataSharesForProducer = DescribeDataSharesForProducer'
  { -- | An identifier giving the status of a datashare in the producer. If this
    -- field is specified, Amazon Redshift returns the list of datashares that
    -- have the specified status.
    status :: Prelude.Maybe DataShareStatusForProducer,
    -- | The Amazon Resource Name (ARN) of the producer that returns in the list
    -- of datashares.
    producerArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of response records to return in each call. If the
    -- number of remaining response records exceeds the specified @MaxRecords@
    -- value, a value is returned in a @marker@ field of the response. You can
    -- retrieve the next set of records by retrying the command with the
    -- returned marker value.
    maxRecords :: Prelude.Maybe Prelude.Int,
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeDataSharesForProducer
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
    marker :: Prelude.Maybe Prelude.Text
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
-- 'status', 'describeDataSharesForProducer_status' - An identifier giving the status of a datashare in the producer. If this
-- field is specified, Amazon Redshift returns the list of datashares that
-- have the specified status.
--
-- 'producerArn', 'describeDataSharesForProducer_producerArn' - The Amazon Resource Name (ARN) of the producer that returns in the list
-- of datashares.
--
-- 'maxRecords', 'describeDataSharesForProducer_maxRecords' - The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
--
-- 'marker', 'describeDataSharesForProducer_marker' - An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForProducer
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
newDescribeDataSharesForProducer ::
  DescribeDataSharesForProducer
newDescribeDataSharesForProducer =
  DescribeDataSharesForProducer'
    { status =
        Prelude.Nothing,
      producerArn = Prelude.Nothing,
      maxRecords = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | An identifier giving the status of a datashare in the producer. If this
-- field is specified, Amazon Redshift returns the list of datashares that
-- have the specified status.
describeDataSharesForProducer_status :: Lens.Lens' DescribeDataSharesForProducer (Prelude.Maybe DataShareStatusForProducer)
describeDataSharesForProducer_status = Lens.lens (\DescribeDataSharesForProducer' {status} -> status) (\s@DescribeDataSharesForProducer' {} a -> s {status = a} :: DescribeDataSharesForProducer)

-- | The Amazon Resource Name (ARN) of the producer that returns in the list
-- of datashares.
describeDataSharesForProducer_producerArn :: Lens.Lens' DescribeDataSharesForProducer (Prelude.Maybe Prelude.Text)
describeDataSharesForProducer_producerArn = Lens.lens (\DescribeDataSharesForProducer' {producerArn} -> producerArn) (\s@DescribeDataSharesForProducer' {} a -> s {producerArn = a} :: DescribeDataSharesForProducer)

-- | The maximum number of response records to return in each call. If the
-- number of remaining response records exceeds the specified @MaxRecords@
-- value, a value is returned in a @marker@ field of the response. You can
-- retrieve the next set of records by retrying the command with the
-- returned marker value.
describeDataSharesForProducer_maxRecords :: Lens.Lens' DescribeDataSharesForProducer (Prelude.Maybe Prelude.Int)
describeDataSharesForProducer_maxRecords = Lens.lens (\DescribeDataSharesForProducer' {maxRecords} -> maxRecords) (\s@DescribeDataSharesForProducer' {} a -> s {maxRecords = a} :: DescribeDataSharesForProducer)

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForProducer
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeDataSharesForProducer_marker :: Lens.Lens' DescribeDataSharesForProducer (Prelude.Maybe Prelude.Text)
describeDataSharesForProducer_marker = Lens.lens (\DescribeDataSharesForProducer' {marker} -> marker) (\s@DescribeDataSharesForProducer' {} a -> s {marker = a} :: DescribeDataSharesForProducer)

instance
  Core.AWSRequest
    DescribeDataSharesForProducer
  where
  type
    AWSResponse DescribeDataSharesForProducer =
      DescribeDataSharesForProducerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeDataSharesForProducerResult"
      ( \s h x ->
          DescribeDataSharesForProducerResponse'
            Prelude.<$> ( x Core..@? "DataShares" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (x Core..@? "Marker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDataSharesForProducer

instance Prelude.NFData DescribeDataSharesForProducer

instance Core.ToHeaders DescribeDataSharesForProducer where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeDataSharesForProducer where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDataSharesForProducer where
  toQuery DescribeDataSharesForProducer' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "DescribeDataSharesForProducer" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2012-12-01" :: Prelude.ByteString),
        "Status" Core.=: status,
        "ProducerArn" Core.=: producerArn,
        "MaxRecords" Core.=: maxRecords,
        "Marker" Core.=: marker
      ]

-- | /See:/ 'newDescribeDataSharesForProducerResponse' smart constructor.
data DescribeDataSharesForProducerResponse = DescribeDataSharesForProducerResponse'
  { -- | Shows the results of datashares available for producers.
    dataShares :: Prelude.Maybe [DataShare],
    -- | An optional parameter that specifies the starting point to return a set
    -- of response records. When the results of a DescribeDataSharesForProducer
    -- request exceed the value specified in @MaxRecords@, AWS returns a value
    -- in the @Marker@ field of the response. You can retrieve the next set of
    -- response records by providing the returned marker value in the @Marker@
    -- parameter and retrying the request.
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
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
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
describeDataSharesForProducerResponse_dataShares = Lens.lens (\DescribeDataSharesForProducerResponse' {dataShares} -> dataShares) (\s@DescribeDataSharesForProducerResponse' {} a -> s {dataShares = a} :: DescribeDataSharesForProducerResponse) Prelude.. Lens.mapping Lens._Coerce

-- | An optional parameter that specifies the starting point to return a set
-- of response records. When the results of a DescribeDataSharesForProducer
-- request exceed the value specified in @MaxRecords@, AWS returns a value
-- in the @Marker@ field of the response. You can retrieve the next set of
-- response records by providing the returned marker value in the @Marker@
-- parameter and retrying the request.
describeDataSharesForProducerResponse_marker :: Lens.Lens' DescribeDataSharesForProducerResponse (Prelude.Maybe Prelude.Text)
describeDataSharesForProducerResponse_marker = Lens.lens (\DescribeDataSharesForProducerResponse' {marker} -> marker) (\s@DescribeDataSharesForProducerResponse' {} a -> s {marker = a} :: DescribeDataSharesForProducerResponse)

-- | The response's http status code.
describeDataSharesForProducerResponse_httpStatus :: Lens.Lens' DescribeDataSharesForProducerResponse Prelude.Int
describeDataSharesForProducerResponse_httpStatus = Lens.lens (\DescribeDataSharesForProducerResponse' {httpStatus} -> httpStatus) (\s@DescribeDataSharesForProducerResponse' {} a -> s {httpStatus = a} :: DescribeDataSharesForProducerResponse)

instance
  Prelude.NFData
    DescribeDataSharesForProducerResponse
