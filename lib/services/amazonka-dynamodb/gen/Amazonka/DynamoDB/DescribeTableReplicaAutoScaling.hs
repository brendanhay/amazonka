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
-- Module      : Amazonka.DynamoDB.DescribeTableReplicaAutoScaling
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes auto scaling settings across replicas of the global table at
-- once.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
-- of global tables.
module Amazonka.DynamoDB.DescribeTableReplicaAutoScaling
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeTableReplicaAutoScalingResponse'
            Prelude.<$> (x Data..?> "TableAutoScalingDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeTableReplicaAutoScaling
  where
  hashWithSalt
    _salt
    DescribeTableReplicaAutoScaling' {..} =
      _salt `Prelude.hashWithSalt` tableName

instance
  Prelude.NFData
    DescribeTableReplicaAutoScaling
  where
  rnf DescribeTableReplicaAutoScaling' {..} =
    Prelude.rnf tableName

instance
  Data.ToHeaders
    DescribeTableReplicaAutoScaling
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeTableReplicaAutoScaling" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeTableReplicaAutoScaling where
  toJSON DescribeTableReplicaAutoScaling' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TableName" Data..= tableName)]
      )

instance Data.ToPath DescribeTableReplicaAutoScaling where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeTableReplicaAutoScaling where
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
  where
  rnf DescribeTableReplicaAutoScalingResponse' {..} =
    Prelude.rnf tableAutoScalingDescription
      `Prelude.seq` Prelude.rnf httpStatus
