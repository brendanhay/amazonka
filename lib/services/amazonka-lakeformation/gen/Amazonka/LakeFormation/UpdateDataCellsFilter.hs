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
-- Module      : Amazonka.LakeFormation.UpdateDataCellsFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a data cell filter.
module Amazonka.LakeFormation.UpdateDataCellsFilter
  ( -- * Creating a Request
    UpdateDataCellsFilter (..),
    newUpdateDataCellsFilter,

    -- * Request Lenses
    updateDataCellsFilter_tableData,

    -- * Destructuring the Response
    UpdateDataCellsFilterResponse (..),
    newUpdateDataCellsFilterResponse,

    -- * Response Lenses
    updateDataCellsFilterResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LakeFormation.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataCellsFilter' smart constructor.
data UpdateDataCellsFilter = UpdateDataCellsFilter'
  { -- | A @DataCellsFilter@ structure containing information about the data
    -- cells filter.
    tableData :: DataCellsFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataCellsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableData', 'updateDataCellsFilter_tableData' - A @DataCellsFilter@ structure containing information about the data
-- cells filter.
newUpdateDataCellsFilter ::
  -- | 'tableData'
  DataCellsFilter ->
  UpdateDataCellsFilter
newUpdateDataCellsFilter pTableData_ =
  UpdateDataCellsFilter' {tableData = pTableData_}

-- | A @DataCellsFilter@ structure containing information about the data
-- cells filter.
updateDataCellsFilter_tableData :: Lens.Lens' UpdateDataCellsFilter DataCellsFilter
updateDataCellsFilter_tableData = Lens.lens (\UpdateDataCellsFilter' {tableData} -> tableData) (\s@UpdateDataCellsFilter' {} a -> s {tableData = a} :: UpdateDataCellsFilter)

instance Core.AWSRequest UpdateDataCellsFilter where
  type
    AWSResponse UpdateDataCellsFilter =
      UpdateDataCellsFilterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDataCellsFilterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataCellsFilter where
  hashWithSalt _salt UpdateDataCellsFilter' {..} =
    _salt `Prelude.hashWithSalt` tableData

instance Prelude.NFData UpdateDataCellsFilter where
  rnf UpdateDataCellsFilter' {..} =
    Prelude.rnf tableData

instance Data.ToHeaders UpdateDataCellsFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDataCellsFilter where
  toJSON UpdateDataCellsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TableData" Data..= tableData)]
      )

instance Data.ToPath UpdateDataCellsFilter where
  toPath = Prelude.const "/UpdateDataCellsFilter"

instance Data.ToQuery UpdateDataCellsFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataCellsFilterResponse' smart constructor.
data UpdateDataCellsFilterResponse = UpdateDataCellsFilterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataCellsFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDataCellsFilterResponse_httpStatus' - The response's http status code.
newUpdateDataCellsFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDataCellsFilterResponse
newUpdateDataCellsFilterResponse pHttpStatus_ =
  UpdateDataCellsFilterResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDataCellsFilterResponse_httpStatus :: Lens.Lens' UpdateDataCellsFilterResponse Prelude.Int
updateDataCellsFilterResponse_httpStatus = Lens.lens (\UpdateDataCellsFilterResponse' {httpStatus} -> httpStatus) (\s@UpdateDataCellsFilterResponse' {} a -> s {httpStatus = a} :: UpdateDataCellsFilterResponse)

instance Prelude.NFData UpdateDataCellsFilterResponse where
  rnf UpdateDataCellsFilterResponse' {..} =
    Prelude.rnf httpStatus
