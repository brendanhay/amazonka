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
-- Module      : Amazonka.DynamoDB.DescribeGlobalTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the specified global table.
--
-- This operation only applies to
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V1.html Version 2017.11.29>
-- of global tables. If you are using global tables
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/globaltables.V2.html Version 2019.11.21>
-- you can use
-- <https://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_DescribeTable.html DescribeTable>
-- instead.
module Amazonka.DynamoDB.DescribeGlobalTable
  ( -- * Creating a Request
    DescribeGlobalTable (..),
    newDescribeGlobalTable,

    -- * Request Lenses
    describeGlobalTable_globalTableName,

    -- * Destructuring the Response
    DescribeGlobalTableResponse (..),
    newDescribeGlobalTableResponse,

    -- * Response Lenses
    describeGlobalTableResponse_globalTableDescription,
    describeGlobalTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGlobalTable' smart constructor.
data DescribeGlobalTable = DescribeGlobalTable'
  { -- | The name of the global table.
    globalTableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalTableName', 'describeGlobalTable_globalTableName' - The name of the global table.
newDescribeGlobalTable ::
  -- | 'globalTableName'
  Prelude.Text ->
  DescribeGlobalTable
newDescribeGlobalTable pGlobalTableName_ =
  DescribeGlobalTable'
    { globalTableName =
        pGlobalTableName_
    }

-- | The name of the global table.
describeGlobalTable_globalTableName :: Lens.Lens' DescribeGlobalTable Prelude.Text
describeGlobalTable_globalTableName = Lens.lens (\DescribeGlobalTable' {globalTableName} -> globalTableName) (\s@DescribeGlobalTable' {} a -> s {globalTableName = a} :: DescribeGlobalTable)

instance Core.AWSRequest DescribeGlobalTable where
  type
    AWSResponse DescribeGlobalTable =
      DescribeGlobalTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGlobalTableResponse'
            Prelude.<$> (x Data..?> "GlobalTableDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGlobalTable where
  hashWithSalt _salt DescribeGlobalTable' {..} =
    _salt `Prelude.hashWithSalt` globalTableName

instance Prelude.NFData DescribeGlobalTable where
  rnf DescribeGlobalTable' {..} =
    Prelude.rnf globalTableName

instance Data.ToHeaders DescribeGlobalTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DynamoDB_20120810.DescribeGlobalTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeGlobalTable where
  toJSON DescribeGlobalTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GlobalTableName" Data..= globalTableName)
          ]
      )

instance Data.ToPath DescribeGlobalTable where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeGlobalTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeGlobalTableResponse' smart constructor.
data DescribeGlobalTableResponse = DescribeGlobalTableResponse'
  { -- | Contains the details of the global table.
    globalTableDescription :: Prelude.Maybe GlobalTableDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'globalTableDescription', 'describeGlobalTableResponse_globalTableDescription' - Contains the details of the global table.
--
-- 'httpStatus', 'describeGlobalTableResponse_httpStatus' - The response's http status code.
newDescribeGlobalTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGlobalTableResponse
newDescribeGlobalTableResponse pHttpStatus_ =
  DescribeGlobalTableResponse'
    { globalTableDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the details of the global table.
describeGlobalTableResponse_globalTableDescription :: Lens.Lens' DescribeGlobalTableResponse (Prelude.Maybe GlobalTableDescription)
describeGlobalTableResponse_globalTableDescription = Lens.lens (\DescribeGlobalTableResponse' {globalTableDescription} -> globalTableDescription) (\s@DescribeGlobalTableResponse' {} a -> s {globalTableDescription = a} :: DescribeGlobalTableResponse)

-- | The response's http status code.
describeGlobalTableResponse_httpStatus :: Lens.Lens' DescribeGlobalTableResponse Prelude.Int
describeGlobalTableResponse_httpStatus = Lens.lens (\DescribeGlobalTableResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalTableResponse' {} a -> s {httpStatus = a} :: DescribeGlobalTableResponse)

instance Prelude.NFData DescribeGlobalTableResponse where
  rnf DescribeGlobalTableResponse' {..} =
    Prelude.rnf globalTableDescription
      `Prelude.seq` Prelude.rnf httpStatus
