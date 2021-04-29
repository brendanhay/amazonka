{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDeleteExperiment' smart constructor.
data DeleteExperiment = DeleteExperiment'
  { -- | The name of the experiment to delete.
    experimentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteExperiment
newDeleteExperiment pExperimentName_ =
  DeleteExperiment'
    { experimentName =
        pExperimentName_
    }

-- | The name of the experiment to delete.
deleteExperiment_experimentName :: Lens.Lens' DeleteExperiment Prelude.Text
deleteExperiment_experimentName = Lens.lens (\DeleteExperiment' {experimentName} -> experimentName) (\s@DeleteExperiment' {} a -> s {experimentName = a} :: DeleteExperiment)

instance Prelude.AWSRequest DeleteExperiment where
  type Rs DeleteExperiment = DeleteExperimentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteExperimentResponse'
            Prelude.<$> (x Prelude..?> "ExperimentArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteExperiment

instance Prelude.NFData DeleteExperiment

instance Prelude.ToHeaders DeleteExperiment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.DeleteExperiment" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteExperiment where
  toJSON DeleteExperiment' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ExperimentName" Prelude..= experimentName)
          ]
      )

instance Prelude.ToPath DeleteExperiment where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteExperiment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteExperimentResponse' smart constructor.
data DeleteExperimentResponse = DeleteExperimentResponse'
  { -- | The Amazon Resource Name (ARN) of the experiment that is being deleted.
    experimentArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteExperimentResponse
newDeleteExperimentResponse pHttpStatus_ =
  DeleteExperimentResponse'
    { experimentArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the experiment that is being deleted.
deleteExperimentResponse_experimentArn :: Lens.Lens' DeleteExperimentResponse (Prelude.Maybe Prelude.Text)
deleteExperimentResponse_experimentArn = Lens.lens (\DeleteExperimentResponse' {experimentArn} -> experimentArn) (\s@DeleteExperimentResponse' {} a -> s {experimentArn = a} :: DeleteExperimentResponse)

-- | The response's http status code.
deleteExperimentResponse_httpStatus :: Lens.Lens' DeleteExperimentResponse Prelude.Int
deleteExperimentResponse_httpStatus = Lens.lens (\DeleteExperimentResponse' {httpStatus} -> httpStatus) (\s@DeleteExperimentResponse' {} a -> s {httpStatus = a} :: DeleteExperimentResponse)

instance Prelude.NFData DeleteExperimentResponse
