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
-- Module      : Amazonka.CodePipeline.PutActionRevision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information to AWS CodePipeline about new revisions to a
-- source.
module Amazonka.CodePipeline.PutActionRevision
  ( -- * Creating a Request
    PutActionRevision (..),
    newPutActionRevision,

    -- * Request Lenses
    putActionRevision_pipelineName,
    putActionRevision_stageName,
    putActionRevision_actionName,
    putActionRevision_actionRevision,

    -- * Destructuring the Response
    PutActionRevisionResponse (..),
    newPutActionRevisionResponse,

    -- * Response Lenses
    putActionRevisionResponse_newRevision,
    putActionRevisionResponse_pipelineExecutionId,
    putActionRevisionResponse_httpStatus,
  )
where

import Amazonka.CodePipeline.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input of a @PutActionRevision@ action.
--
-- /See:/ 'newPutActionRevision' smart constructor.
data PutActionRevision = PutActionRevision'
  { -- | The name of the pipeline that starts processing the revision to the
    -- source.
    pipelineName :: Prelude.Text,
    -- | The name of the stage that contains the action that acts on the
    -- revision.
    stageName :: Prelude.Text,
    -- | The name of the action that processes the revision.
    actionName :: Prelude.Text,
    -- | Represents information about the version (or revision) of an action.
    actionRevision :: ActionRevision
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutActionRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineName', 'putActionRevision_pipelineName' - The name of the pipeline that starts processing the revision to the
-- source.
--
-- 'stageName', 'putActionRevision_stageName' - The name of the stage that contains the action that acts on the
-- revision.
--
-- 'actionName', 'putActionRevision_actionName' - The name of the action that processes the revision.
--
-- 'actionRevision', 'putActionRevision_actionRevision' - Represents information about the version (or revision) of an action.
newPutActionRevision ::
  -- | 'pipelineName'
  Prelude.Text ->
  -- | 'stageName'
  Prelude.Text ->
  -- | 'actionName'
  Prelude.Text ->
  -- | 'actionRevision'
  ActionRevision ->
  PutActionRevision
newPutActionRevision
  pPipelineName_
  pStageName_
  pActionName_
  pActionRevision_ =
    PutActionRevision'
      { pipelineName = pPipelineName_,
        stageName = pStageName_,
        actionName = pActionName_,
        actionRevision = pActionRevision_
      }

-- | The name of the pipeline that starts processing the revision to the
-- source.
putActionRevision_pipelineName :: Lens.Lens' PutActionRevision Prelude.Text
putActionRevision_pipelineName = Lens.lens (\PutActionRevision' {pipelineName} -> pipelineName) (\s@PutActionRevision' {} a -> s {pipelineName = a} :: PutActionRevision)

-- | The name of the stage that contains the action that acts on the
-- revision.
putActionRevision_stageName :: Lens.Lens' PutActionRevision Prelude.Text
putActionRevision_stageName = Lens.lens (\PutActionRevision' {stageName} -> stageName) (\s@PutActionRevision' {} a -> s {stageName = a} :: PutActionRevision)

-- | The name of the action that processes the revision.
putActionRevision_actionName :: Lens.Lens' PutActionRevision Prelude.Text
putActionRevision_actionName = Lens.lens (\PutActionRevision' {actionName} -> actionName) (\s@PutActionRevision' {} a -> s {actionName = a} :: PutActionRevision)

-- | Represents information about the version (or revision) of an action.
putActionRevision_actionRevision :: Lens.Lens' PutActionRevision ActionRevision
putActionRevision_actionRevision = Lens.lens (\PutActionRevision' {actionRevision} -> actionRevision) (\s@PutActionRevision' {} a -> s {actionRevision = a} :: PutActionRevision)

instance Core.AWSRequest PutActionRevision where
  type
    AWSResponse PutActionRevision =
      PutActionRevisionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutActionRevisionResponse'
            Prelude.<$> (x Data..?> "newRevision")
            Prelude.<*> (x Data..?> "pipelineExecutionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutActionRevision where
  hashWithSalt _salt PutActionRevision' {..} =
    _salt
      `Prelude.hashWithSalt` pipelineName
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` actionName
      `Prelude.hashWithSalt` actionRevision

instance Prelude.NFData PutActionRevision where
  rnf PutActionRevision' {..} =
    Prelude.rnf pipelineName `Prelude.seq`
      Prelude.rnf stageName `Prelude.seq`
        Prelude.rnf actionName `Prelude.seq`
          Prelude.rnf actionRevision

instance Data.ToHeaders PutActionRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodePipeline_20150709.PutActionRevision" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutActionRevision where
  toJSON PutActionRevision' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("pipelineName" Data..= pipelineName),
            Prelude.Just ("stageName" Data..= stageName),
            Prelude.Just ("actionName" Data..= actionName),
            Prelude.Just
              ("actionRevision" Data..= actionRevision)
          ]
      )

instance Data.ToPath PutActionRevision where
  toPath = Prelude.const "/"

instance Data.ToQuery PutActionRevision where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the output of a @PutActionRevision@ action.
--
-- /See:/ 'newPutActionRevisionResponse' smart constructor.
data PutActionRevisionResponse = PutActionRevisionResponse'
  { -- | Indicates whether the artifact revision was previously used in an
    -- execution of the specified pipeline.
    newRevision' :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the current workflow state of the pipeline.
    pipelineExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutActionRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newRevision'', 'putActionRevisionResponse_newRevision' - Indicates whether the artifact revision was previously used in an
-- execution of the specified pipeline.
--
-- 'pipelineExecutionId', 'putActionRevisionResponse_pipelineExecutionId' - The ID of the current workflow state of the pipeline.
--
-- 'httpStatus', 'putActionRevisionResponse_httpStatus' - The response's http status code.
newPutActionRevisionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutActionRevisionResponse
newPutActionRevisionResponse pHttpStatus_ =
  PutActionRevisionResponse'
    { newRevision' =
        Prelude.Nothing,
      pipelineExecutionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Indicates whether the artifact revision was previously used in an
-- execution of the specified pipeline.
putActionRevisionResponse_newRevision :: Lens.Lens' PutActionRevisionResponse (Prelude.Maybe Prelude.Bool)
putActionRevisionResponse_newRevision = Lens.lens (\PutActionRevisionResponse' {newRevision'} -> newRevision') (\s@PutActionRevisionResponse' {} a -> s {newRevision' = a} :: PutActionRevisionResponse)

-- | The ID of the current workflow state of the pipeline.
putActionRevisionResponse_pipelineExecutionId :: Lens.Lens' PutActionRevisionResponse (Prelude.Maybe Prelude.Text)
putActionRevisionResponse_pipelineExecutionId = Lens.lens (\PutActionRevisionResponse' {pipelineExecutionId} -> pipelineExecutionId) (\s@PutActionRevisionResponse' {} a -> s {pipelineExecutionId = a} :: PutActionRevisionResponse)

-- | The response's http status code.
putActionRevisionResponse_httpStatus :: Lens.Lens' PutActionRevisionResponse Prelude.Int
putActionRevisionResponse_httpStatus = Lens.lens (\PutActionRevisionResponse' {httpStatus} -> httpStatus) (\s@PutActionRevisionResponse' {} a -> s {httpStatus = a} :: PutActionRevisionResponse)

instance Prelude.NFData PutActionRevisionResponse where
  rnf PutActionRevisionResponse' {..} =
    Prelude.rnf newRevision' `Prelude.seq`
      Prelude.rnf pipelineExecutionId `Prelude.seq`
        Prelude.rnf httpStatus
