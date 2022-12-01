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
-- Module      : Amazonka.Evidently.DeleteExperiment
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Evidently experiment. The feature used for the experiment is
-- not deleted.
--
-- To stop an experiment without deleting it, use
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_StopExperiment.html StopExperiment>.
module Amazonka.Evidently.DeleteExperiment
  ( -- * Creating a Request
    DeleteExperiment (..),
    newDeleteExperiment,

    -- * Request Lenses
    deleteExperiment_experiment,
    deleteExperiment_project,

    -- * Destructuring the Response
    DeleteExperimentResponse (..),
    newDeleteExperimentResponse,

    -- * Response Lenses
    deleteExperimentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteExperiment' smart constructor.
data DeleteExperiment = DeleteExperiment'
  { -- | The name of the experiment to delete.
    experiment :: Prelude.Text,
    -- | The name or ARN of the project that contains the experiment to delete.
    project :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experiment', 'deleteExperiment_experiment' - The name of the experiment to delete.
--
-- 'project', 'deleteExperiment_project' - The name or ARN of the project that contains the experiment to delete.
newDeleteExperiment ::
  -- | 'experiment'
  Prelude.Text ->
  -- | 'project'
  Prelude.Text ->
  DeleteExperiment
newDeleteExperiment pExperiment_ pProject_ =
  DeleteExperiment'
    { experiment = pExperiment_,
      project = pProject_
    }

-- | The name of the experiment to delete.
deleteExperiment_experiment :: Lens.Lens' DeleteExperiment Prelude.Text
deleteExperiment_experiment = Lens.lens (\DeleteExperiment' {experiment} -> experiment) (\s@DeleteExperiment' {} a -> s {experiment = a} :: DeleteExperiment)

-- | The name or ARN of the project that contains the experiment to delete.
deleteExperiment_project :: Lens.Lens' DeleteExperiment Prelude.Text
deleteExperiment_project = Lens.lens (\DeleteExperiment' {project} -> project) (\s@DeleteExperiment' {} a -> s {project = a} :: DeleteExperiment)

instance Core.AWSRequest DeleteExperiment where
  type
    AWSResponse DeleteExperiment =
      DeleteExperimentResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteExperimentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteExperiment where
  hashWithSalt _salt DeleteExperiment' {..} =
    _salt `Prelude.hashWithSalt` experiment
      `Prelude.hashWithSalt` project

instance Prelude.NFData DeleteExperiment where
  rnf DeleteExperiment' {..} =
    Prelude.rnf experiment
      `Prelude.seq` Prelude.rnf project

instance Core.ToHeaders DeleteExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteExperiment where
  toPath DeleteExperiment' {..} =
    Prelude.mconcat
      [ "/projects/",
        Core.toBS project,
        "/experiments/",
        Core.toBS experiment
      ]

instance Core.ToQuery DeleteExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteExperimentResponse' smart constructor.
data DeleteExperimentResponse = DeleteExperimentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteExperimentResponse_httpStatus' - The response's http status code.
newDeleteExperimentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteExperimentResponse
newDeleteExperimentResponse pHttpStatus_ =
  DeleteExperimentResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteExperimentResponse_httpStatus :: Lens.Lens' DeleteExperimentResponse Prelude.Int
deleteExperimentResponse_httpStatus = Lens.lens (\DeleteExperimentResponse' {httpStatus} -> httpStatus) (\s@DeleteExperimentResponse' {} a -> s {httpStatus = a} :: DeleteExperimentResponse)

instance Prelude.NFData DeleteExperimentResponse where
  rnf DeleteExperimentResponse' {..} =
    Prelude.rnf httpStatus
