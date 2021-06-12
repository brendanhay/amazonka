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

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTimeToLive' smart constructor.
data DescribeTimeToLive = DescribeTimeToLive'
  { -- | The name of the table to be described.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeTimeToLive
newDescribeTimeToLive pTableName_ =
  DescribeTimeToLive' {tableName = pTableName_}

-- | The name of the table to be described.
describeTimeToLive_tableName :: Lens.Lens' DescribeTimeToLive Core.Text
describeTimeToLive_tableName = Lens.lens (\DescribeTimeToLive' {tableName} -> tableName) (\s@DescribeTimeToLive' {} a -> s {tableName = a} :: DescribeTimeToLive)

instance Core.AWSRequest DescribeTimeToLive where
  type
    AWSResponse DescribeTimeToLive =
      DescribeTimeToLiveResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTimeToLiveResponse'
            Core.<$> (x Core..?> "TimeToLiveDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTimeToLive

instance Core.NFData DescribeTimeToLive

instance Core.ToHeaders DescribeTimeToLive where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.DescribeTimeToLive" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeTimeToLive where
  toJSON DescribeTimeToLive' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TableName" Core..= tableName)]
      )

instance Core.ToPath DescribeTimeToLive where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTimeToLive where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeTimeToLiveResponse' smart constructor.
data DescribeTimeToLiveResponse = DescribeTimeToLiveResponse'
  { timeToLiveDescription :: Core.Maybe TimeToLiveDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeTimeToLiveResponse
newDescribeTimeToLiveResponse pHttpStatus_ =
  DescribeTimeToLiveResponse'
    { timeToLiveDescription =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
describeTimeToLiveResponse_timeToLiveDescription :: Lens.Lens' DescribeTimeToLiveResponse (Core.Maybe TimeToLiveDescription)
describeTimeToLiveResponse_timeToLiveDescription = Lens.lens (\DescribeTimeToLiveResponse' {timeToLiveDescription} -> timeToLiveDescription) (\s@DescribeTimeToLiveResponse' {} a -> s {timeToLiveDescription = a} :: DescribeTimeToLiveResponse)

-- | The response's http status code.
describeTimeToLiveResponse_httpStatus :: Lens.Lens' DescribeTimeToLiveResponse Core.Int
describeTimeToLiveResponse_httpStatus = Lens.lens (\DescribeTimeToLiveResponse' {httpStatus} -> httpStatus) (\s@DescribeTimeToLiveResponse' {} a -> s {httpStatus = a} :: DescribeTimeToLiveResponse)

instance Core.NFData DescribeTimeToLiveResponse
