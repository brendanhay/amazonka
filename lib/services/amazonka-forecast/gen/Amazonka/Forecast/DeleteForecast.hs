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
-- Module      : Amazonka.Forecast.DeleteForecast
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a forecast created using the CreateForecast operation. You can
-- delete only forecasts that have a status of @ACTIVE@ or @CREATE_FAILED@.
-- To get the status, use the DescribeForecast operation.
--
-- You can\'t delete a forecast while it is being exported. After a
-- forecast is deleted, you can no longer query the forecast.
module Amazonka.Forecast.DeleteForecast
  ( -- * Creating a Request
    DeleteForecast (..),
    newDeleteForecast,

    -- * Request Lenses
    deleteForecast_forecastArn,

    -- * Destructuring the Response
    DeleteForecastResponse (..),
    newDeleteForecastResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteForecast' smart constructor.
data DeleteForecast = DeleteForecast'
  { -- | The Amazon Resource Name (ARN) of the forecast to delete.
    forecastArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastArn', 'deleteForecast_forecastArn' - The Amazon Resource Name (ARN) of the forecast to delete.
newDeleteForecast ::
  -- | 'forecastArn'
  Prelude.Text ->
  DeleteForecast
newDeleteForecast pForecastArn_ =
  DeleteForecast' {forecastArn = pForecastArn_}

-- | The Amazon Resource Name (ARN) of the forecast to delete.
deleteForecast_forecastArn :: Lens.Lens' DeleteForecast Prelude.Text
deleteForecast_forecastArn = Lens.lens (\DeleteForecast' {forecastArn} -> forecastArn) (\s@DeleteForecast' {} a -> s {forecastArn = a} :: DeleteForecast)

instance Core.AWSRequest DeleteForecast where
  type
    AWSResponse DeleteForecast =
      DeleteForecastResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteForecastResponse'

instance Prelude.Hashable DeleteForecast where
  hashWithSalt _salt DeleteForecast' {..} =
    _salt `Prelude.hashWithSalt` forecastArn

instance Prelude.NFData DeleteForecast where
  rnf DeleteForecast' {..} = Prelude.rnf forecastArn

instance Data.ToHeaders DeleteForecast where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DeleteForecast" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteForecast where
  toJSON DeleteForecast' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ForecastArn" Data..= forecastArn)]
      )

instance Data.ToPath DeleteForecast where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteForecast where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteForecastResponse' smart constructor.
data DeleteForecastResponse = DeleteForecastResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteForecastResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteForecastResponse ::
  DeleteForecastResponse
newDeleteForecastResponse = DeleteForecastResponse'

instance Prelude.NFData DeleteForecastResponse where
  rnf _ = ()
