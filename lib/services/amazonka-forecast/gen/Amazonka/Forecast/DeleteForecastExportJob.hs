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
-- Module      : Amazonka.Forecast.DeleteForecastExportJob
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a forecast export job created using the CreateForecastExportJob
-- operation. You can delete only export jobs that have a status of
-- @ACTIVE@ or @CREATE_FAILED@. To get the status, use the
-- DescribeForecastExportJob operation.
module Amazonka.Forecast.DeleteForecastExportJob
  ( -- * Creating a Request
    DeleteForecastExportJob (..),
    newDeleteForecastExportJob,

    -- * Request Lenses
    deleteForecastExportJob_forecastExportJobArn,

    -- * Destructuring the Response
    DeleteForecastExportJobResponse (..),
    newDeleteForecastExportJobResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteForecastExportJob' smart constructor.
data DeleteForecastExportJob = DeleteForecastExportJob'
  { -- | The Amazon Resource Name (ARN) of the forecast export job to delete.
    forecastExportJobArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteForecastExportJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastExportJobArn', 'deleteForecastExportJob_forecastExportJobArn' - The Amazon Resource Name (ARN) of the forecast export job to delete.
newDeleteForecastExportJob ::
  -- | 'forecastExportJobArn'
  Prelude.Text ->
  DeleteForecastExportJob
newDeleteForecastExportJob pForecastExportJobArn_ =
  DeleteForecastExportJob'
    { forecastExportJobArn =
        pForecastExportJobArn_
    }

-- | The Amazon Resource Name (ARN) of the forecast export job to delete.
deleteForecastExportJob_forecastExportJobArn :: Lens.Lens' DeleteForecastExportJob Prelude.Text
deleteForecastExportJob_forecastExportJobArn = Lens.lens (\DeleteForecastExportJob' {forecastExportJobArn} -> forecastExportJobArn) (\s@DeleteForecastExportJob' {} a -> s {forecastExportJobArn = a} :: DeleteForecastExportJob)

instance Core.AWSRequest DeleteForecastExportJob where
  type
    AWSResponse DeleteForecastExportJob =
      DeleteForecastExportJobResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteForecastExportJobResponse'

instance Prelude.Hashable DeleteForecastExportJob where
  hashWithSalt _salt DeleteForecastExportJob' {..} =
    _salt `Prelude.hashWithSalt` forecastExportJobArn

instance Prelude.NFData DeleteForecastExportJob where
  rnf DeleteForecastExportJob' {..} =
    Prelude.rnf forecastExportJobArn

instance Data.ToHeaders DeleteForecastExportJob where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DeleteForecastExportJob" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteForecastExportJob where
  toJSON DeleteForecastExportJob' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ForecastExportJobArn"
                  Data..= forecastExportJobArn
              )
          ]
      )

instance Data.ToPath DeleteForecastExportJob where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteForecastExportJob where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteForecastExportJobResponse' smart constructor.
data DeleteForecastExportJobResponse = DeleteForecastExportJobResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteForecastExportJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteForecastExportJobResponse ::
  DeleteForecastExportJobResponse
newDeleteForecastExportJobResponse =
  DeleteForecastExportJobResponse'

instance
  Prelude.NFData
    DeleteForecastExportJobResponse
  where
  rnf _ = ()
