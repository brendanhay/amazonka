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
-- Module      : Amazonka.Forecast.DeleteWhatIfAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a what-if analysis created using the CreateWhatIfAnalysis
-- operation. You can delete only what-if analyses that have a status of
-- @ACTIVE@ or @CREATE_FAILED@. To get the status, use the
-- DescribeWhatIfAnalysis operation.
--
-- You can\'t delete a what-if analysis while any of its forecasts are
-- being exported.
module Amazonka.Forecast.DeleteWhatIfAnalysis
  ( -- * Creating a Request
    DeleteWhatIfAnalysis (..),
    newDeleteWhatIfAnalysis,

    -- * Request Lenses
    deleteWhatIfAnalysis_whatIfAnalysisArn,

    -- * Destructuring the Response
    DeleteWhatIfAnalysisResponse (..),
    newDeleteWhatIfAnalysisResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteWhatIfAnalysis' smart constructor.
data DeleteWhatIfAnalysis = DeleteWhatIfAnalysis'
  { -- | The Amazon Resource Name (ARN) of the what-if analysis that you want to
    -- delete.
    whatIfAnalysisArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWhatIfAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'whatIfAnalysisArn', 'deleteWhatIfAnalysis_whatIfAnalysisArn' - The Amazon Resource Name (ARN) of the what-if analysis that you want to
-- delete.
newDeleteWhatIfAnalysis ::
  -- | 'whatIfAnalysisArn'
  Prelude.Text ->
  DeleteWhatIfAnalysis
newDeleteWhatIfAnalysis pWhatIfAnalysisArn_ =
  DeleteWhatIfAnalysis'
    { whatIfAnalysisArn =
        pWhatIfAnalysisArn_
    }

-- | The Amazon Resource Name (ARN) of the what-if analysis that you want to
-- delete.
deleteWhatIfAnalysis_whatIfAnalysisArn :: Lens.Lens' DeleteWhatIfAnalysis Prelude.Text
deleteWhatIfAnalysis_whatIfAnalysisArn = Lens.lens (\DeleteWhatIfAnalysis' {whatIfAnalysisArn} -> whatIfAnalysisArn) (\s@DeleteWhatIfAnalysis' {} a -> s {whatIfAnalysisArn = a} :: DeleteWhatIfAnalysis)

instance Core.AWSRequest DeleteWhatIfAnalysis where
  type
    AWSResponse DeleteWhatIfAnalysis =
      DeleteWhatIfAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteWhatIfAnalysisResponse'

instance Prelude.Hashable DeleteWhatIfAnalysis where
  hashWithSalt _salt DeleteWhatIfAnalysis' {..} =
    _salt `Prelude.hashWithSalt` whatIfAnalysisArn

instance Prelude.NFData DeleteWhatIfAnalysis where
  rnf DeleteWhatIfAnalysis' {..} =
    Prelude.rnf whatIfAnalysisArn

instance Data.ToHeaders DeleteWhatIfAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DeleteWhatIfAnalysis" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteWhatIfAnalysis where
  toJSON DeleteWhatIfAnalysis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WhatIfAnalysisArn" Data..= whatIfAnalysisArn)
          ]
      )

instance Data.ToPath DeleteWhatIfAnalysis where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteWhatIfAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteWhatIfAnalysisResponse' smart constructor.
data DeleteWhatIfAnalysisResponse = DeleteWhatIfAnalysisResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteWhatIfAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteWhatIfAnalysisResponse ::
  DeleteWhatIfAnalysisResponse
newDeleteWhatIfAnalysisResponse =
  DeleteWhatIfAnalysisResponse'

instance Prelude.NFData DeleteWhatIfAnalysisResponse where
  rnf _ = ()
