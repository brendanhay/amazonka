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
-- Module      : Amazonka.EMR.StopNotebookExecution
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a notebook execution.
module Amazonka.EMR.StopNotebookExecution
  ( -- * Creating a Request
    StopNotebookExecution (..),
    newStopNotebookExecution,

    -- * Request Lenses
    stopNotebookExecution_notebookExecutionId,

    -- * Destructuring the Response
    StopNotebookExecutionResponse (..),
    newStopNotebookExecutionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStopNotebookExecution' smart constructor.
data StopNotebookExecution = StopNotebookExecution'
  { -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopNotebookExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookExecutionId', 'stopNotebookExecution_notebookExecutionId' - The unique identifier of the notebook execution.
newStopNotebookExecution ::
  -- | 'notebookExecutionId'
  Prelude.Text ->
  StopNotebookExecution
newStopNotebookExecution pNotebookExecutionId_ =
  StopNotebookExecution'
    { notebookExecutionId =
        pNotebookExecutionId_
    }

-- | The unique identifier of the notebook execution.
stopNotebookExecution_notebookExecutionId :: Lens.Lens' StopNotebookExecution Prelude.Text
stopNotebookExecution_notebookExecutionId = Lens.lens (\StopNotebookExecution' {notebookExecutionId} -> notebookExecutionId) (\s@StopNotebookExecution' {} a -> s {notebookExecutionId = a} :: StopNotebookExecution)

instance Core.AWSRequest StopNotebookExecution where
  type
    AWSResponse StopNotebookExecution =
      StopNotebookExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull StopNotebookExecutionResponse'

instance Prelude.Hashable StopNotebookExecution where
  hashWithSalt _salt StopNotebookExecution' {..} =
    _salt `Prelude.hashWithSalt` notebookExecutionId

instance Prelude.NFData StopNotebookExecution where
  rnf StopNotebookExecution' {..} =
    Prelude.rnf notebookExecutionId

instance Data.ToHeaders StopNotebookExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.StopNotebookExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StopNotebookExecution where
  toJSON StopNotebookExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("NotebookExecutionId" Data..= notebookExecutionId)
          ]
      )

instance Data.ToPath StopNotebookExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery StopNotebookExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopNotebookExecutionResponse' smart constructor.
data StopNotebookExecutionResponse = StopNotebookExecutionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StopNotebookExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopNotebookExecutionResponse ::
  StopNotebookExecutionResponse
newStopNotebookExecutionResponse =
  StopNotebookExecutionResponse'

instance Prelude.NFData StopNotebookExecutionResponse where
  rnf _ = ()
