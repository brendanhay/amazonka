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
-- Module      : Amazonka.Forecast.DeleteExplainability
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Explainability resource.
--
-- You can delete only predictor that have a status of @ACTIVE@ or
-- @CREATE_FAILED@. To get the status, use the DescribeExplainability
-- operation.
module Amazonka.Forecast.DeleteExplainability
  ( -- * Creating a Request
    DeleteExplainability (..),
    newDeleteExplainability,

    -- * Request Lenses
    deleteExplainability_explainabilityArn,

    -- * Destructuring the Response
    DeleteExplainabilityResponse (..),
    newDeleteExplainabilityResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteExplainability' smart constructor.
data DeleteExplainability = DeleteExplainability'
  { -- | The Amazon Resource Name (ARN) of the Explainability resource to delete.
    explainabilityArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExplainability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'explainabilityArn', 'deleteExplainability_explainabilityArn' - The Amazon Resource Name (ARN) of the Explainability resource to delete.
newDeleteExplainability ::
  -- | 'explainabilityArn'
  Prelude.Text ->
  DeleteExplainability
newDeleteExplainability pExplainabilityArn_ =
  DeleteExplainability'
    { explainabilityArn =
        pExplainabilityArn_
    }

-- | The Amazon Resource Name (ARN) of the Explainability resource to delete.
deleteExplainability_explainabilityArn :: Lens.Lens' DeleteExplainability Prelude.Text
deleteExplainability_explainabilityArn = Lens.lens (\DeleteExplainability' {explainabilityArn} -> explainabilityArn) (\s@DeleteExplainability' {} a -> s {explainabilityArn = a} :: DeleteExplainability)

instance Core.AWSRequest DeleteExplainability where
  type
    AWSResponse DeleteExplainability =
      DeleteExplainabilityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteExplainabilityResponse'

instance Prelude.Hashable DeleteExplainability where
  hashWithSalt _salt DeleteExplainability' {..} =
    _salt `Prelude.hashWithSalt` explainabilityArn

instance Prelude.NFData DeleteExplainability where
  rnf DeleteExplainability' {..} =
    Prelude.rnf explainabilityArn

instance Data.ToHeaders DeleteExplainability where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.DeleteExplainability" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteExplainability where
  toJSON DeleteExplainability' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ExplainabilityArn" Data..= explainabilityArn)
          ]
      )

instance Data.ToPath DeleteExplainability where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteExplainability where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteExplainabilityResponse' smart constructor.
data DeleteExplainabilityResponse = DeleteExplainabilityResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExplainabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteExplainabilityResponse ::
  DeleteExplainabilityResponse
newDeleteExplainabilityResponse =
  DeleteExplainabilityResponse'

instance Prelude.NFData DeleteExplainabilityResponse where
  rnf _ = ()
