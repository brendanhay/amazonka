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
-- Module      : Amazonka.DevOpsGuru.StartCostEstimation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the creation of an estimate of the monthly cost to analyze your
-- Amazon Web Services resources.
module Amazonka.DevOpsGuru.StartCostEstimation
  ( -- * Creating a Request
    StartCostEstimation (..),
    newStartCostEstimation,

    -- * Request Lenses
    startCostEstimation_clientToken,
    startCostEstimation_resourceCollection,

    -- * Destructuring the Response
    StartCostEstimationResponse (..),
    newStartCostEstimationResponse,

    -- * Response Lenses
    startCostEstimationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartCostEstimation' smart constructor.
data StartCostEstimation = StartCostEstimation'
  { -- | The idempotency token used to identify each cost estimate request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The collection of Amazon Web Services resources used to create a monthly
    -- DevOps Guru cost estimate.
    resourceCollection :: CostEstimationResourceCollectionFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCostEstimation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startCostEstimation_clientToken' - The idempotency token used to identify each cost estimate request.
--
-- 'resourceCollection', 'startCostEstimation_resourceCollection' - The collection of Amazon Web Services resources used to create a monthly
-- DevOps Guru cost estimate.
newStartCostEstimation ::
  -- | 'resourceCollection'
  CostEstimationResourceCollectionFilter ->
  StartCostEstimation
newStartCostEstimation pResourceCollection_ =
  StartCostEstimation'
    { clientToken = Prelude.Nothing,
      resourceCollection = pResourceCollection_
    }

-- | The idempotency token used to identify each cost estimate request.
startCostEstimation_clientToken :: Lens.Lens' StartCostEstimation (Prelude.Maybe Prelude.Text)
startCostEstimation_clientToken = Lens.lens (\StartCostEstimation' {clientToken} -> clientToken) (\s@StartCostEstimation' {} a -> s {clientToken = a} :: StartCostEstimation)

-- | The collection of Amazon Web Services resources used to create a monthly
-- DevOps Guru cost estimate.
startCostEstimation_resourceCollection :: Lens.Lens' StartCostEstimation CostEstimationResourceCollectionFilter
startCostEstimation_resourceCollection = Lens.lens (\StartCostEstimation' {resourceCollection} -> resourceCollection) (\s@StartCostEstimation' {} a -> s {resourceCollection = a} :: StartCostEstimation)

instance Core.AWSRequest StartCostEstimation where
  type
    AWSResponse StartCostEstimation =
      StartCostEstimationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartCostEstimationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable StartCostEstimation where
  hashWithSalt _salt StartCostEstimation' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` resourceCollection

instance Prelude.NFData StartCostEstimation where
  rnf StartCostEstimation' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf resourceCollection

instance Data.ToHeaders StartCostEstimation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartCostEstimation where
  toJSON StartCostEstimation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just
              ("ResourceCollection" Data..= resourceCollection)
          ]
      )

instance Data.ToPath StartCostEstimation where
  toPath = Prelude.const "/cost-estimation"

instance Data.ToQuery StartCostEstimation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartCostEstimationResponse' smart constructor.
data StartCostEstimationResponse = StartCostEstimationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartCostEstimationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startCostEstimationResponse_httpStatus' - The response's http status code.
newStartCostEstimationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  StartCostEstimationResponse
newStartCostEstimationResponse pHttpStatus_ =
  StartCostEstimationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
startCostEstimationResponse_httpStatus :: Lens.Lens' StartCostEstimationResponse Prelude.Int
startCostEstimationResponse_httpStatus = Lens.lens (\StartCostEstimationResponse' {httpStatus} -> httpStatus) (\s@StartCostEstimationResponse' {} a -> s {httpStatus = a} :: StartCostEstimationResponse)

instance Prelude.NFData StartCostEstimationResponse where
  rnf StartCostEstimationResponse' {..} =
    Prelude.rnf httpStatus
