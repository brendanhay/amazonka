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
-- Module      : Amazonka.SecurityLake.UpdateDataLake
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies where to store your security data and for how long. You can
-- add a rollup Region to consolidate data from multiple Amazon Web
-- Services Regions.
module Amazonka.SecurityLake.UpdateDataLake
  ( -- * Creating a Request
    UpdateDataLake (..),
    newUpdateDataLake,

    -- * Request Lenses
    updateDataLake_configurations,

    -- * Destructuring the Response
    UpdateDataLakeResponse (..),
    newUpdateDataLakeResponse,

    -- * Response Lenses
    updateDataLakeResponse_dataLakes,
    updateDataLakeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newUpdateDataLake' smart constructor.
data UpdateDataLake = UpdateDataLake'
  { -- | Specify the Region or Regions that will contribute data to the rollup
    -- region.
    configurations :: [DataLakeConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataLake' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurations', 'updateDataLake_configurations' - Specify the Region or Regions that will contribute data to the rollup
-- region.
newUpdateDataLake ::
  UpdateDataLake
newUpdateDataLake =
  UpdateDataLake' {configurations = Prelude.mempty}

-- | Specify the Region or Regions that will contribute data to the rollup
-- region.
updateDataLake_configurations :: Lens.Lens' UpdateDataLake [DataLakeConfiguration]
updateDataLake_configurations = Lens.lens (\UpdateDataLake' {configurations} -> configurations) (\s@UpdateDataLake' {} a -> s {configurations = a} :: UpdateDataLake) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateDataLake where
  type
    AWSResponse UpdateDataLake =
      UpdateDataLakeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDataLakeResponse'
            Prelude.<$> (x Data..?> "dataLakes" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataLake where
  hashWithSalt _salt UpdateDataLake' {..} =
    _salt `Prelude.hashWithSalt` configurations

instance Prelude.NFData UpdateDataLake where
  rnf UpdateDataLake' {..} = Prelude.rnf configurations

instance Data.ToHeaders UpdateDataLake where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDataLake where
  toJSON UpdateDataLake' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("configurations" Data..= configurations)
          ]
      )

instance Data.ToPath UpdateDataLake where
  toPath = Prelude.const "/v1/datalake"

instance Data.ToQuery UpdateDataLake where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataLakeResponse' smart constructor.
data UpdateDataLakeResponse = UpdateDataLakeResponse'
  { -- | The created Security Lake configuration object.
    dataLakes :: Prelude.Maybe [DataLakeResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataLakeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataLakes', 'updateDataLakeResponse_dataLakes' - The created Security Lake configuration object.
--
-- 'httpStatus', 'updateDataLakeResponse_httpStatus' - The response's http status code.
newUpdateDataLakeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDataLakeResponse
newUpdateDataLakeResponse pHttpStatus_ =
  UpdateDataLakeResponse'
    { dataLakes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The created Security Lake configuration object.
updateDataLakeResponse_dataLakes :: Lens.Lens' UpdateDataLakeResponse (Prelude.Maybe [DataLakeResource])
updateDataLakeResponse_dataLakes = Lens.lens (\UpdateDataLakeResponse' {dataLakes} -> dataLakes) (\s@UpdateDataLakeResponse' {} a -> s {dataLakes = a} :: UpdateDataLakeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateDataLakeResponse_httpStatus :: Lens.Lens' UpdateDataLakeResponse Prelude.Int
updateDataLakeResponse_httpStatus = Lens.lens (\UpdateDataLakeResponse' {httpStatus} -> httpStatus) (\s@UpdateDataLakeResponse' {} a -> s {httpStatus = a} :: UpdateDataLakeResponse)

instance Prelude.NFData UpdateDataLakeResponse where
  rnf UpdateDataLakeResponse' {..} =
    Prelude.rnf dataLakes
      `Prelude.seq` Prelude.rnf httpStatus
