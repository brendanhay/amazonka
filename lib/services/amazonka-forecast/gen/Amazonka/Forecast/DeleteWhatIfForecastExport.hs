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
-- Module      : Amazonka.Forecast.DeleteWhatIfForecastExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a what-if forecast export created using the
-- CreateWhatIfForecastExport operation. You can delete only what-if
-- forecast exports that have a status of @ACTIVE@ or @CREATE_FAILED@. To
-- get the status, use the DescribeWhatIfForecastExport operation.
module Amazonka.Forecast.DeleteWhatIfForecastExport
  ( -- * Creating a Request
    DeleteWhatIfForecastExport (..),
    newDeleteWhatIfForecastExport,

    -- * Request Lenses
    deleteWhatIfForecastExport_whatIfForecastExportArn,

    -- * Destructuring the Response
    DeleteWhatIfForecastExportResponse (..),
    newDeleteWhatIfForecastExportResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWhatIfForecastExport' smart constructor.
data DeleteWhatIfForecastExport = DeleteWhatIfForecastExport'
  { -- | The Amazon Resource Name (ARN) of the what-if forecast export that you
    -- want to delete.
    whatIfForecastExportArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWhatIfForecastExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'whatIfForecastExportArn', 'deleteWhatIfForecastExport_whatIfForecastExportArn' - The Amazon Resource Name (ARN) of the what-if forecast export that you
-- want to delete.
newDeleteWhatIfForecastExport ::
  -- | 'whatIfForecastExportArn'
  Prelude.Text ->
  DeleteWhatIfForecastExport
newDeleteWhatIfForecastExport
  pWhatIfForecastExportArn_ =
    DeleteWhatIfForecastExport'
      { whatIfForecastExportArn =
          pWhatIfForecastExportArn_
      }

-- | The Amazon Resource Name (ARN) of the what-if forecast export that you
-- want to delete.
deleteWhatIfForecastExport_whatIfForecastExportArn :: Lens.Lens' DeleteWhatIfForecastExport Prelude.Text
deleteWhatIfForecastExport_whatIfForecastExportArn = Lens.lens (\DeleteWhatIfForecastExport' {whatIfForecastExportArn} -> whatIfForecastExportArn) (\s@DeleteWhatIfForecastExport' {} a -> s {whatIfForecastExportArn = a} :: DeleteWhatIfForecastExport)

instance Core.AWSRequest DeleteWhatIfForecastExport where
  type
    AWSResponse DeleteWhatIfForecastExport =
      DeleteWhatIfForecastExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteWhatIfForecastExportResponse'

instance Prelude.Hashable DeleteWhatIfForecastExport where
  hashWithSalt _salt DeleteWhatIfForecastExport' {..} =
    _salt
      `Prelude.hashWithSalt` whatIfForecastExportArn

instance Prelude.NFData DeleteWhatIfForecastExport where
  rnf DeleteWhatIfForecastExport' {..} =
    Prelude.rnf whatIfForecastExportArn

instance Data.ToHeaders DeleteWhatIfForecastExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DeleteWhatIfForecastExport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWhatIfForecastExport where
  toJSON DeleteWhatIfForecastExport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "WhatIfForecastExportArn"
                  Data..= whatIfForecastExportArn
              )
          ]
      )

instance Data.ToPath DeleteWhatIfForecastExport where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWhatIfForecastExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWhatIfForecastExportResponse' smart constructor.
data DeleteWhatIfForecastExportResponse = DeleteWhatIfForecastExportResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWhatIfForecastExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteWhatIfForecastExportResponse ::
  DeleteWhatIfForecastExportResponse
newDeleteWhatIfForecastExportResponse =
  DeleteWhatIfForecastExportResponse'

instance
  Prelude.NFData
    DeleteWhatIfForecastExportResponse
  where
  rnf _ = ()
