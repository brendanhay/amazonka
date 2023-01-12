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
-- Module      : Amazonka.Forecast.DeleteExplainabilityExport
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Explainability export.
module Amazonka.Forecast.DeleteExplainabilityExport
  ( -- * Creating a Request
    DeleteExplainabilityExport (..),
    newDeleteExplainabilityExport,

    -- * Request Lenses
    deleteExplainabilityExport_explainabilityExportArn,

    -- * Destructuring the Response
    DeleteExplainabilityExportResponse (..),
    newDeleteExplainabilityExportResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteExplainabilityExport' smart constructor.
data DeleteExplainabilityExport = DeleteExplainabilityExport'
  { -- | The Amazon Resource Name (ARN) of the Explainability export to delete.
    explainabilityExportArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExplainabilityExport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'explainabilityExportArn', 'deleteExplainabilityExport_explainabilityExportArn' - The Amazon Resource Name (ARN) of the Explainability export to delete.
newDeleteExplainabilityExport ::
  -- | 'explainabilityExportArn'
  Prelude.Text ->
  DeleteExplainabilityExport
newDeleteExplainabilityExport
  pExplainabilityExportArn_ =
    DeleteExplainabilityExport'
      { explainabilityExportArn =
          pExplainabilityExportArn_
      }

-- | The Amazon Resource Name (ARN) of the Explainability export to delete.
deleteExplainabilityExport_explainabilityExportArn :: Lens.Lens' DeleteExplainabilityExport Prelude.Text
deleteExplainabilityExport_explainabilityExportArn = Lens.lens (\DeleteExplainabilityExport' {explainabilityExportArn} -> explainabilityExportArn) (\s@DeleteExplainabilityExport' {} a -> s {explainabilityExportArn = a} :: DeleteExplainabilityExport)

instance Core.AWSRequest DeleteExplainabilityExport where
  type
    AWSResponse DeleteExplainabilityExport =
      DeleteExplainabilityExportResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteExplainabilityExportResponse'

instance Prelude.Hashable DeleteExplainabilityExport where
  hashWithSalt _salt DeleteExplainabilityExport' {..} =
    _salt
      `Prelude.hashWithSalt` explainabilityExportArn

instance Prelude.NFData DeleteExplainabilityExport where
  rnf DeleteExplainabilityExport' {..} =
    Prelude.rnf explainabilityExportArn

instance Data.ToHeaders DeleteExplainabilityExport where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DeleteExplainabilityExport" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteExplainabilityExport where
  toJSON DeleteExplainabilityExport' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ExplainabilityExportArn"
                  Data..= explainabilityExportArn
              )
          ]
      )

instance Data.ToPath DeleteExplainabilityExport where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteExplainabilityExport where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteExplainabilityExportResponse' smart constructor.
data DeleteExplainabilityExportResponse = DeleteExplainabilityExportResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExplainabilityExportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteExplainabilityExportResponse ::
  DeleteExplainabilityExportResponse
newDeleteExplainabilityExportResponse =
  DeleteExplainabilityExportResponse'

instance
  Prelude.NFData
    DeleteExplainabilityExportResponse
  where
  rnf _ = ()
