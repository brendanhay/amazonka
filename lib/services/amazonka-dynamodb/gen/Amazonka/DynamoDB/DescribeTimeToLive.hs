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
-- Module      : Amazonka.DynamoDB.DescribeTimeToLive
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gives a description of the Time to Live (TTL) status on the specified
-- table.
module Amazonka.DynamoDB.DescribeTimeToLive
  ( -- * Creating a Request
    DescribeTimeToLive (..),
    newDescribeTimeToLive,

    -- * Request Lenses
    describeTimeToLive_tableName,

    -- * Destructuring the Response
    DescribeTimeToLiveResponse (..),
    newDescribeTimeToLiveResponse,

    -- * Response Lenses
    describeTimeToLiveResponse_timeToLiveDescription,
    describeTimeToLiveResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeTimeToLive' smart constructor.
data DescribeTimeToLive = DescribeTimeToLive'
  { -- | The name of the table to be described.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTimeToLive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'describeTimeToLive_tableName' - The name of the table to be described.
newDescribeTimeToLive ::
  -- | 'tableName'
  Prelude.Text ->
  DescribeTimeToLive
newDescribeTimeToLive pTableName_ =
  DescribeTimeToLive' {tableName = pTableName_}

-- | The name of the table to be described.
describeTimeToLive_tableName :: Lens.Lens' DescribeTimeToLive Prelude.Text
describeTimeToLive_tableName = Lens.lens (\DescribeTimeToLive' {tableName} -> tableName) (\s@DescribeTimeToLive' {} a -> s {tableName = a} :: DescribeTimeToLive)

instance Core.AWSRequest DescribeTimeToLive where
  type
    AWSResponse DescribeTimeToLive =
      DescribeTimeToLiveResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTimeToLiveResponse'
            Prelude.<$> (x Data..?> "TimeToLiveDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTimeToLive where
  hashWithSalt _salt DescribeTimeToLive' {..} =
    _salt `Prelude.hashWithSalt` tableName

instance Prelude.NFData DescribeTimeToLive where
  rnf DescribeTimeToLive' {..} = Prelude.rnf tableName

instance Data.ToHeaders DescribeTimeToLive where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeTimeToLive" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTimeToLive where
  toJSON DescribeTimeToLive' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TableName" Data..= tableName)]
      )

instance Data.ToPath DescribeTimeToLive where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTimeToLive where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTimeToLiveResponse' smart constructor.
data DescribeTimeToLiveResponse = DescribeTimeToLiveResponse'
  { timeToLiveDescription :: Prelude.Maybe TimeToLiveDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTimeToLiveResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeToLiveDescription', 'describeTimeToLiveResponse_timeToLiveDescription' -
--
-- 'httpStatus', 'describeTimeToLiveResponse_httpStatus' - The response's http status code.
newDescribeTimeToLiveResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTimeToLiveResponse
newDescribeTimeToLiveResponse pHttpStatus_ =
  DescribeTimeToLiveResponse'
    { timeToLiveDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
describeTimeToLiveResponse_timeToLiveDescription :: Lens.Lens' DescribeTimeToLiveResponse (Prelude.Maybe TimeToLiveDescription)
describeTimeToLiveResponse_timeToLiveDescription = Lens.lens (\DescribeTimeToLiveResponse' {timeToLiveDescription} -> timeToLiveDescription) (\s@DescribeTimeToLiveResponse' {} a -> s {timeToLiveDescription = a} :: DescribeTimeToLiveResponse)

-- | The response's http status code.
describeTimeToLiveResponse_httpStatus :: Lens.Lens' DescribeTimeToLiveResponse Prelude.Int
describeTimeToLiveResponse_httpStatus = Lens.lens (\DescribeTimeToLiveResponse' {httpStatus} -> httpStatus) (\s@DescribeTimeToLiveResponse' {} a -> s {httpStatus = a} :: DescribeTimeToLiveResponse)

instance Prelude.NFData DescribeTimeToLiveResponse where
  rnf DescribeTimeToLiveResponse' {..} =
    Prelude.rnf timeToLiveDescription
      `Prelude.seq` Prelude.rnf httpStatus
