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
-- Module      : Amazonka.Forecast.DeletePredictor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a predictor created using the DescribePredictor or
-- CreatePredictor operations. You can delete only predictor that have a
-- status of @ACTIVE@ or @CREATE_FAILED@. To get the status, use the
-- DescribePredictor operation.
module Amazonka.Forecast.DeletePredictor
  ( -- * Creating a Request
    DeletePredictor (..),
    newDeletePredictor,

    -- * Request Lenses
    deletePredictor_predictorArn,

    -- * Destructuring the Response
    DeletePredictorResponse (..),
    newDeletePredictorResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePredictor' smart constructor.
data DeletePredictor = DeletePredictor'
  { -- | The Amazon Resource Name (ARN) of the predictor to delete.
    predictorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePredictor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'predictorArn', 'deletePredictor_predictorArn' - The Amazon Resource Name (ARN) of the predictor to delete.
newDeletePredictor ::
  -- | 'predictorArn'
  Prelude.Text ->
  DeletePredictor
newDeletePredictor pPredictorArn_ =
  DeletePredictor' {predictorArn = pPredictorArn_}

-- | The Amazon Resource Name (ARN) of the predictor to delete.
deletePredictor_predictorArn :: Lens.Lens' DeletePredictor Prelude.Text
deletePredictor_predictorArn = Lens.lens (\DeletePredictor' {predictorArn} -> predictorArn) (\s@DeletePredictor' {} a -> s {predictorArn = a} :: DeletePredictor)

instance Core.AWSRequest DeletePredictor where
  type
    AWSResponse DeletePredictor =
      DeletePredictorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeletePredictorResponse'

instance Prelude.Hashable DeletePredictor where
  hashWithSalt _salt DeletePredictor' {..} =
    _salt `Prelude.hashWithSalt` predictorArn

instance Prelude.NFData DeletePredictor where
  rnf DeletePredictor' {..} = Prelude.rnf predictorArn

instance Data.ToHeaders DeletePredictor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DeletePredictor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePredictor where
  toJSON DeletePredictor' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("PredictorArn" Data..= predictorArn)]
      )

instance Data.ToPath DeletePredictor where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePredictor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePredictorResponse' smart constructor.
data DeletePredictorResponse = DeletePredictorResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeletePredictorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePredictorResponse ::
  DeletePredictorResponse
newDeletePredictorResponse = DeletePredictorResponse'

instance Prelude.NFData DeletePredictorResponse where
  rnf _ = ()
