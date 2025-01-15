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
-- Module      : Amazonka.TimeStreamQuery.DescribeScheduledQuery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides detailed information about a scheduled query.
module Amazonka.TimeStreamQuery.DescribeScheduledQuery
  ( -- * Creating a Request
    DescribeScheduledQuery (..),
    newDescribeScheduledQuery,

    -- * Request Lenses
    describeScheduledQuery_scheduledQueryArn,

    -- * Destructuring the Response
    DescribeScheduledQueryResponse (..),
    newDescribeScheduledQueryResponse,

    -- * Response Lenses
    describeScheduledQueryResponse_httpStatus,
    describeScheduledQueryResponse_scheduledQuery,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamQuery.Types

-- | /See:/ 'newDescribeScheduledQuery' smart constructor.
data DescribeScheduledQuery = DescribeScheduledQuery'
  { -- | The ARN of the scheduled query.
    scheduledQueryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScheduledQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledQueryArn', 'describeScheduledQuery_scheduledQueryArn' - The ARN of the scheduled query.
newDescribeScheduledQuery ::
  -- | 'scheduledQueryArn'
  Prelude.Text ->
  DescribeScheduledQuery
newDescribeScheduledQuery pScheduledQueryArn_ =
  DescribeScheduledQuery'
    { scheduledQueryArn =
        pScheduledQueryArn_
    }

-- | The ARN of the scheduled query.
describeScheduledQuery_scheduledQueryArn :: Lens.Lens' DescribeScheduledQuery Prelude.Text
describeScheduledQuery_scheduledQueryArn = Lens.lens (\DescribeScheduledQuery' {scheduledQueryArn} -> scheduledQueryArn) (\s@DescribeScheduledQuery' {} a -> s {scheduledQueryArn = a} :: DescribeScheduledQuery)

instance Core.AWSRequest DescribeScheduledQuery where
  type
    AWSResponse DescribeScheduledQuery =
      DescribeScheduledQueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScheduledQueryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ScheduledQuery")
      )

instance Prelude.Hashable DescribeScheduledQuery where
  hashWithSalt _salt DescribeScheduledQuery' {..} =
    _salt `Prelude.hashWithSalt` scheduledQueryArn

instance Prelude.NFData DescribeScheduledQuery where
  rnf DescribeScheduledQuery' {..} =
    Prelude.rnf scheduledQueryArn

instance Data.ToHeaders DescribeScheduledQuery where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Timestream_20181101.DescribeScheduledQuery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeScheduledQuery where
  toJSON DescribeScheduledQuery' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ScheduledQueryArn" Data..= scheduledQueryArn)
          ]
      )

instance Data.ToPath DescribeScheduledQuery where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeScheduledQuery where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeScheduledQueryResponse' smart constructor.
data DescribeScheduledQueryResponse = DescribeScheduledQueryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The scheduled query.
    scheduledQuery :: ScheduledQueryDescription
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeScheduledQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeScheduledQueryResponse_httpStatus' - The response's http status code.
--
-- 'scheduledQuery', 'describeScheduledQueryResponse_scheduledQuery' - The scheduled query.
newDescribeScheduledQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'scheduledQuery'
  ScheduledQueryDescription ->
  DescribeScheduledQueryResponse
newDescribeScheduledQueryResponse
  pHttpStatus_
  pScheduledQuery_ =
    DescribeScheduledQueryResponse'
      { httpStatus =
          pHttpStatus_,
        scheduledQuery = pScheduledQuery_
      }

-- | The response's http status code.
describeScheduledQueryResponse_httpStatus :: Lens.Lens' DescribeScheduledQueryResponse Prelude.Int
describeScheduledQueryResponse_httpStatus = Lens.lens (\DescribeScheduledQueryResponse' {httpStatus} -> httpStatus) (\s@DescribeScheduledQueryResponse' {} a -> s {httpStatus = a} :: DescribeScheduledQueryResponse)

-- | The scheduled query.
describeScheduledQueryResponse_scheduledQuery :: Lens.Lens' DescribeScheduledQueryResponse ScheduledQueryDescription
describeScheduledQueryResponse_scheduledQuery = Lens.lens (\DescribeScheduledQueryResponse' {scheduledQuery} -> scheduledQuery) (\s@DescribeScheduledQueryResponse' {} a -> s {scheduledQuery = a} :: DescribeScheduledQueryResponse)

instance
  Prelude.NFData
    DescribeScheduledQueryResponse
  where
  rnf DescribeScheduledQueryResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf scheduledQuery
