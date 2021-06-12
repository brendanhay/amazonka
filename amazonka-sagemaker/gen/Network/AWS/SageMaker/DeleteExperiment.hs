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
-- Module      : Network.AWS.SageMaker.DeleteExperiment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an Amazon SageMaker experiment. All trials associated with the
-- experiment must be deleted first. Use the ListTrials API to get a list
-- of the trials associated with the experiment.
module Network.AWS.SageMaker.DeleteExperiment
  ( -- * Creating a Request
    DeleteExperiment (..),
    newDeleteExperiment,

    -- * Request Lenses
    deleteExperiment_experimentName,

    -- * Destructuring the Response
    DeleteExperimentResponse (..),
    newDeleteExperimentResponse,

    -- * Response Lenses
    deleteExperimentResponse_experimentArn,
    deleteExperimentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteExperiment' smart constructor.
data DeleteExperiment = DeleteExperiment'
  { -- | The name of the experiment to delete.
    experimentName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteExperiment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentName', 'deleteExperiment_experimentName' - The name of the experiment to delete.
newDeleteExperiment ::
  -- | 'experimentName'
  Core.Text ->
  DeleteExperiment
newDeleteExperiment pExperimentName_ =
  DeleteExperiment'
    { experimentName =
        pExperimentName_
    }

-- | The name of the experiment to delete.
deleteExperiment_experimentName :: Lens.Lens' DeleteExperiment Core.Text
deleteExperiment_experimentName = Lens.lens (\DeleteExperiment' {experimentName} -> experimentName) (\s@DeleteExperiment' {} a -> s {experimentName = a} :: DeleteExperiment)

instance Core.AWSRequest DeleteExperiment where
  type
    AWSResponse DeleteExperiment =
      DeleteExperimentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteExperimentResponse'
            Core.<$> (x Core..?> "ExperimentArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteExperiment

instance Core.NFData DeleteExperiment

instance Core.ToHeaders DeleteExperiment where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DeleteExperiment" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteExperiment where
  toJSON DeleteExperiment' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ExperimentName" Core..= experimentName)
          ]
      )

instance Core.ToPath DeleteExperiment where
  toPath = Core.const "/"

instance Core.ToQuery DeleteExperiment where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteExperimentResponse' smart constructor.
data DeleteExperimentResponse = DeleteExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment that is being deleted.
    experimentArn :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteExperimentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'experimentArn', 'deleteExperimentResponse_experimentArn' - The Amazon Resource Name (ARN) of the experiment that is being deleted.
--
-- 'httpStatus', 'deleteExperimentResponse_httpStatus' - The response's http status code.
newDeleteExperimentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteExperimentResponse
newDeleteExperimentResponse pHttpStatus_ =
  DeleteExperimentResponse'
    { experimentArn =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment that is being deleted.
deleteExperimentResponse_experimentArn :: Lens.Lens' DeleteExperimentResponse (Core.Maybe Core.Text)
deleteExperimentResponse_experimentArn = Lens.lens (\DeleteExperimentResponse' {experimentArn} -> experimentArn) (\s@DeleteExperimentResponse' {} a -> s {experimentArn = a} :: DeleteExperimentResponse)

-- | The response's http status code.
deleteExperimentResponse_httpStatus :: Lens.Lens' DeleteExperimentResponse Core.Int
deleteExperimentResponse_httpStatus = Lens.lens (\DeleteExperimentResponse' {httpStatus} -> httpStatus) (\s@DeleteExperimentResponse' {} a -> s {httpStatus = a} :: DeleteExperimentResponse)

instance Core.NFData DeleteExperimentResponse
