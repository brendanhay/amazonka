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
-- Module      : Amazonka.Forecast.DeleteWhatIfForecast
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a what-if forecast created using the CreateWhatIfForecast
-- operation. You can delete only what-if forecasts that have a status of
-- @ACTIVE@ or @CREATE_FAILED@. To get the status, use the
-- DescribeWhatIfForecast operation.
--
-- You can\'t delete a what-if forecast while it is being exported. After a
-- what-if forecast is deleted, you can no longer query the what-if
-- analysis.
module Amazonka.Forecast.DeleteWhatIfForecast
  ( -- * Creating a Request
    DeleteWhatIfForecast (..),
    newDeleteWhatIfForecast,

    -- * Request Lenses
    deleteWhatIfForecast_whatIfForecastArn,

    -- * Destructuring the Response
    DeleteWhatIfForecastResponse (..),
    newDeleteWhatIfForecastResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWhatIfForecast' smart constructor.
data DeleteWhatIfForecast = DeleteWhatIfForecast'
  { -- | The Amazon Resource Name (ARN) of the what-if forecast that you want to
    -- delete.
    whatIfForecastArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWhatIfForecast' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'whatIfForecastArn', 'deleteWhatIfForecast_whatIfForecastArn' - The Amazon Resource Name (ARN) of the what-if forecast that you want to
-- delete.
newDeleteWhatIfForecast ::
  -- | 'whatIfForecastArn'
  Prelude.Text ->
  DeleteWhatIfForecast
newDeleteWhatIfForecast pWhatIfForecastArn_ =
  DeleteWhatIfForecast'
    { whatIfForecastArn =
        pWhatIfForecastArn_
    }

-- | The Amazon Resource Name (ARN) of the what-if forecast that you want to
-- delete.
deleteWhatIfForecast_whatIfForecastArn :: Lens.Lens' DeleteWhatIfForecast Prelude.Text
deleteWhatIfForecast_whatIfForecastArn = Lens.lens (\DeleteWhatIfForecast' {whatIfForecastArn} -> whatIfForecastArn) (\s@DeleteWhatIfForecast' {} a -> s {whatIfForecastArn = a} :: DeleteWhatIfForecast)

instance Core.AWSRequest DeleteWhatIfForecast where
  type
    AWSResponse DeleteWhatIfForecast =
      DeleteWhatIfForecastResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteWhatIfForecastResponse'

instance Prelude.Hashable DeleteWhatIfForecast where
  hashWithSalt _salt DeleteWhatIfForecast' {..} =
    _salt `Prelude.hashWithSalt` whatIfForecastArn

instance Prelude.NFData DeleteWhatIfForecast where
  rnf DeleteWhatIfForecast' {..} =
    Prelude.rnf whatIfForecastArn

instance Core.ToHeaders DeleteWhatIfForecast where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonForecast.DeleteWhatIfForecast" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteWhatIfForecast where
  toJSON DeleteWhatIfForecast' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WhatIfForecastArn" Core..= whatIfForecastArn)
          ]
      )

instance Core.ToPath DeleteWhatIfForecast where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteWhatIfForecast where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWhatIfForecastResponse' smart constructor.
data DeleteWhatIfForecastResponse = DeleteWhatIfForecastResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWhatIfForecastResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteWhatIfForecastResponse ::
  DeleteWhatIfForecastResponse
newDeleteWhatIfForecastResponse =
  DeleteWhatIfForecastResponse'

instance Prelude.NFData DeleteWhatIfForecastResponse where
  rnf _ = ()
