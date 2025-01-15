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
-- Module      : Amazonka.FIS.GetExperiment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified experiment.
module Amazonka.FIS.GetExperiment
  ( -- * Creating a Request
    GetExperiment (..),
    newGetExperiment,

    -- * Request Lenses
    getExperiment_id,

    -- * Destructuring the Response
    GetExperimentResponse (..),
    newGetExperimentResponse,

    -- * Response Lenses
    getExperimentResponse_experiment,
    getExperimentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FIS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetExperiment' smart constructor.
data GetExperiment = GetExperiment'
  { -- | The ID of the experiment.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getExperiment_id' - The ID of the experiment.
newGetExperiment ::
  -- | 'id'
  Prelude.Text ->
  GetExperiment
newGetExperiment pId_ = GetExperiment' {id = pId_}

-- | The ID of the experiment.
getExperiment_id :: Lens.Lens' GetExperiment Prelude.Text
getExperiment_id = Lens.lens (\GetExperiment' {id} -> id) (\s@GetExperiment' {} a -> s {id = a} :: GetExperiment)

instance Core.AWSRequest GetExperiment where
  type
    AWSResponse GetExperiment =
      GetExperimentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExperimentResponse'
            Prelude.<$> (x Data..?> "experiment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExperiment where
  hashWithSalt _salt GetExperiment' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetExperiment where
  rnf GetExperiment' {..} = Prelude.rnf id

instance Data.ToHeaders GetExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetExperiment where
  toPath GetExperiment' {..} =
    Prelude.mconcat ["/experiments/", Data.toBS id]

instance Data.ToQuery GetExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExperimentResponse' smart constructor.
data GetExperimentResponse = GetExperimentResponse'
  { -- | Information about the experiment.
    experiment :: Prelude.Maybe Experiment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experiment', 'getExperimentResponse_experiment' - Information about the experiment.
--
-- 'httpStatus', 'getExperimentResponse_httpStatus' - The response's http status code.
newGetExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetExperimentResponse
newGetExperimentResponse pHttpStatus_ =
  GetExperimentResponse'
    { experiment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the experiment.
getExperimentResponse_experiment :: Lens.Lens' GetExperimentResponse (Prelude.Maybe Experiment)
getExperimentResponse_experiment = Lens.lens (\GetExperimentResponse' {experiment} -> experiment) (\s@GetExperimentResponse' {} a -> s {experiment = a} :: GetExperimentResponse)

-- | The response's http status code.
getExperimentResponse_httpStatus :: Lens.Lens' GetExperimentResponse Prelude.Int
getExperimentResponse_httpStatus = Lens.lens (\GetExperimentResponse' {httpStatus} -> httpStatus) (\s@GetExperimentResponse' {} a -> s {httpStatus = a} :: GetExperimentResponse)

instance Prelude.NFData GetExperimentResponse where
  rnf GetExperimentResponse' {..} =
    Prelude.rnf experiment `Prelude.seq`
      Prelude.rnf httpStatus
