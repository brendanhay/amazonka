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
-- Module      : Network.AWS.EMR.StopNotebookExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a notebook execution.
module Network.AWS.EMR.StopNotebookExecution
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

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopNotebookExecution' smart constructor.
data StopNotebookExecution = StopNotebookExecution'
  { -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  StopNotebookExecution
newStopNotebookExecution pNotebookExecutionId_ =
  StopNotebookExecution'
    { notebookExecutionId =
        pNotebookExecutionId_
    }

-- | The unique identifier of the notebook execution.
stopNotebookExecution_notebookExecutionId :: Lens.Lens' StopNotebookExecution Core.Text
stopNotebookExecution_notebookExecutionId = Lens.lens (\StopNotebookExecution' {notebookExecutionId} -> notebookExecutionId) (\s@StopNotebookExecution' {} a -> s {notebookExecutionId = a} :: StopNotebookExecution)

instance Core.AWSRequest StopNotebookExecution where
  type
    AWSResponse StopNotebookExecution =
      StopNotebookExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StopNotebookExecutionResponse'

instance Core.Hashable StopNotebookExecution

instance Core.NFData StopNotebookExecution

instance Core.ToHeaders StopNotebookExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.StopNotebookExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopNotebookExecution where
  toJSON StopNotebookExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("NotebookExecutionId" Core..= notebookExecutionId)
          ]
      )

instance Core.ToPath StopNotebookExecution where
  toPath = Core.const "/"

instance Core.ToQuery StopNotebookExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopNotebookExecutionResponse' smart constructor.
data StopNotebookExecutionResponse = StopNotebookExecutionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopNotebookExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopNotebookExecutionResponse ::
  StopNotebookExecutionResponse
newStopNotebookExecutionResponse =
  StopNotebookExecutionResponse'

instance Core.NFData StopNotebookExecutionResponse
