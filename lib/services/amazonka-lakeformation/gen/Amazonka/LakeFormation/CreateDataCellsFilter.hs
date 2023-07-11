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
-- Module      : Amazonka.LakeFormation.CreateDataCellsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data cell filter to allow one to grant access to certain
-- columns on certain rows.
module Amazonka.LakeFormation.CreateDataCellsFilter
  ( -- * Creating a Request
    CreateDataCellsFilter (..),
    newCreateDataCellsFilter,

    -- * Request Lenses
    createDataCellsFilter_tableData,

    -- * Destructuring the Response
    CreateDataCellsFilterResponse (..),
    newCreateDataCellsFilterResponse,

    -- * Response Lenses
    createDataCellsFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataCellsFilter' smart constructor.
data CreateDataCellsFilter = CreateDataCellsFilter'
  { -- | A @DataCellsFilter@ structure containing information about the data
    -- cells filter.
    tableData :: DataCellsFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataCellsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableData', 'createDataCellsFilter_tableData' - A @DataCellsFilter@ structure containing information about the data
-- cells filter.
newCreateDataCellsFilter ::
  -- | 'tableData'
  DataCellsFilter ->
  CreateDataCellsFilter
newCreateDataCellsFilter pTableData_ =
  CreateDataCellsFilter' {tableData = pTableData_}

-- | A @DataCellsFilter@ structure containing information about the data
-- cells filter.
createDataCellsFilter_tableData :: Lens.Lens' CreateDataCellsFilter DataCellsFilter
createDataCellsFilter_tableData = Lens.lens (\CreateDataCellsFilter' {tableData} -> tableData) (\s@CreateDataCellsFilter' {} a -> s {tableData = a} :: CreateDataCellsFilter)

instance Core.AWSRequest CreateDataCellsFilter where
  type
    AWSResponse CreateDataCellsFilter =
      CreateDataCellsFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDataCellsFilterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataCellsFilter where
  hashWithSalt _salt CreateDataCellsFilter' {..} =
    _salt `Prelude.hashWithSalt` tableData

instance Prelude.NFData CreateDataCellsFilter where
  rnf CreateDataCellsFilter' {..} =
    Prelude.rnf tableData

instance Data.ToHeaders CreateDataCellsFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataCellsFilter where
  toJSON CreateDataCellsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TableData" Data..= tableData)]
      )

instance Data.ToPath CreateDataCellsFilter where
  toPath = Prelude.const "/CreateDataCellsFilter"

instance Data.ToQuery CreateDataCellsFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataCellsFilterResponse' smart constructor.
data CreateDataCellsFilterResponse = CreateDataCellsFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataCellsFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDataCellsFilterResponse_httpStatus' - The response's http status code.
newCreateDataCellsFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataCellsFilterResponse
newCreateDataCellsFilterResponse pHttpStatus_ =
  CreateDataCellsFilterResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createDataCellsFilterResponse_httpStatus :: Lens.Lens' CreateDataCellsFilterResponse Prelude.Int
createDataCellsFilterResponse_httpStatus = Lens.lens (\CreateDataCellsFilterResponse' {httpStatus} -> httpStatus) (\s@CreateDataCellsFilterResponse' {} a -> s {httpStatus = a} :: CreateDataCellsFilterResponse)

instance Prelude.NFData CreateDataCellsFilterResponse where
  rnf CreateDataCellsFilterResponse' {..} =
    Prelude.rnf httpStatus
