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
-- Module      : Network.AWS.DynamoDB.DescribeTableReplicaAutoScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes auto scaling settings across replicas of the global table at
-- once.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
-- of global tables.
module Network.AWS.DynamoDB.DescribeTableReplicaAutoScaling
  ( -- * Creating a Request
    DescribeTableReplicaAutoScaling (..),
    newDescribeTableReplicaAutoScaling,

    -- * Request Lenses
    describeTableReplicaAutoScaling_tableName,

    -- * Destructuring the Response
    DescribeTableReplicaAutoScalingResponse (..),
    newDescribeTableReplicaAutoScalingResponse,

    -- * Response Lenses
    describeTableReplicaAutoScalingResponse_tableAutoScalingDescription,
    describeTableReplicaAutoScalingResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DynamoDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTableReplicaAutoScaling' smart constructor.
data DescribeTableReplicaAutoScaling = DescribeTableReplicaAutoScaling'
  { -- | The name of the table.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableReplicaAutoScaling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableName', 'describeTableReplicaAutoScaling_tableName' - The name of the table.
newDescribeTableReplicaAutoScaling ::
  -- | 'tableName'
  Prelude.Text ->
  DescribeTableReplicaAutoScaling
newDescribeTableReplicaAutoScaling pTableName_ =
  DescribeTableReplicaAutoScaling'
    { tableName =
        pTableName_
    }

-- | The name of the table.
describeTableReplicaAutoScaling_tableName :: Lens.Lens' DescribeTableReplicaAutoScaling Prelude.Text
describeTableReplicaAutoScaling_tableName = Lens.lens (\DescribeTableReplicaAutoScaling' {tableName} -> tableName) (\s@DescribeTableReplicaAutoScaling' {} a -> s {tableName = a} :: DescribeTableReplicaAutoScaling)

instance
  Core.AWSRequest
    DescribeTableReplicaAutoScaling
  where
  type
    AWSResponse DescribeTableReplicaAutoScaling =
      DescribeTableReplicaAutoScalingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTableReplicaAutoScalingResponse'
            Prelude.<$> (x Core..?> "TableAutoScalingDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTableReplicaAutoScaling

instance
  Prelude.NFData
    DescribeTableReplicaAutoScaling

instance
  Core.ToHeaders
    DescribeTableReplicaAutoScaling
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DynamoDB_20120810.DescribeTableReplicaAutoScaling" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeTableReplicaAutoScaling where
  toJSON DescribeTableReplicaAutoScaling' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("TableName" Core..= tableName)]
      )

instance Core.ToPath DescribeTableReplicaAutoScaling where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeTableReplicaAutoScaling where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeTableReplicaAutoScalingResponse' smart constructor.
data DescribeTableReplicaAutoScalingResponse = DescribeTableReplicaAutoScalingResponse'
  { -- | Represents the auto scaling properties of the table.
    tableAutoScalingDescription :: Prelude.Maybe TableAutoScalingDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeTableReplicaAutoScalingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableAutoScalingDescription', 'describeTableReplicaAutoScalingResponse_tableAutoScalingDescription' - Represents the auto scaling properties of the table.
--
-- 'httpStatus', 'describeTableReplicaAutoScalingResponse_httpStatus' - The response's http status code.
newDescribeTableReplicaAutoScalingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeTableReplicaAutoScalingResponse
newDescribeTableReplicaAutoScalingResponse
  pHttpStatus_ =
    DescribeTableReplicaAutoScalingResponse'
      { tableAutoScalingDescription =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Represents the auto scaling properties of the table.
describeTableReplicaAutoScalingResponse_tableAutoScalingDescription :: Lens.Lens' DescribeTableReplicaAutoScalingResponse (Prelude.Maybe TableAutoScalingDescription)
describeTableReplicaAutoScalingResponse_tableAutoScalingDescription = Lens.lens (\DescribeTableReplicaAutoScalingResponse' {tableAutoScalingDescription} -> tableAutoScalingDescription) (\s@DescribeTableReplicaAutoScalingResponse' {} a -> s {tableAutoScalingDescription = a} :: DescribeTableReplicaAutoScalingResponse)

-- | The response's http status code.
describeTableReplicaAutoScalingResponse_httpStatus :: Lens.Lens' DescribeTableReplicaAutoScalingResponse Prelude.Int
describeTableReplicaAutoScalingResponse_httpStatus = Lens.lens (\DescribeTableReplicaAutoScalingResponse' {httpStatus} -> httpStatus) (\s@DescribeTableReplicaAutoScalingResponse' {} a -> s {httpStatus = a} :: DescribeTableReplicaAutoScalingResponse)

instance
  Prelude.NFData
    DescribeTableReplicaAutoScalingResponse
