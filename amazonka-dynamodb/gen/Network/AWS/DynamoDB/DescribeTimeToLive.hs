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
-- Module      : Network.AWS.DynamoDB.DescribeTimeToLive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gives a description of the Time to Live (TTL) status on the specified
-- table.
module Network.AWS.DynamoDB.DescribeTimeToLive
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

import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTimeToLive' smart constructor.
data DescribeTimeToLive = DescribeTimeToLive'
  { -- | The name of the table to be described.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DescribeTimeToLive where
  type
    Rs DescribeTimeToLive =
      DescribeTimeToLiveResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTimeToLiveResponse'
            Prelude.<$> (x Prelude..?> "TimeToLiveDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeTimeToLive

instance Prelude.NFData DescribeTimeToLive

instance Prelude.ToHeaders DescribeTimeToLive where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DynamoDB_20120810.DescribeTimeToLive" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.0" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeTimeToLive where
  toJSON DescribeTimeToLive' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("TableName" Prelude..= tableName)]
      )

instance Prelude.ToPath DescribeTimeToLive where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeTimeToLive where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTimeToLiveResponse' smart constructor.
data DescribeTimeToLiveResponse = DescribeTimeToLiveResponse'
  { timeToLiveDescription :: Prelude.Maybe TimeToLiveDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DescribeTimeToLiveResponse
