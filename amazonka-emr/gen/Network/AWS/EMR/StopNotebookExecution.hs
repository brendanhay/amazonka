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

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopNotebookExecution' smart constructor.
data StopNotebookExecution = StopNotebookExecution'
  { -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest StopNotebookExecution where
  type
    Rs StopNotebookExecution =
      StopNotebookExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull StopNotebookExecutionResponse'

instance Prelude.Hashable StopNotebookExecution

instance Prelude.NFData StopNotebookExecution

instance Prelude.ToHeaders StopNotebookExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "ElasticMapReduce.StopNotebookExecution" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON StopNotebookExecution where
  toJSON StopNotebookExecution' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "NotebookExecutionId"
                  Prelude..= notebookExecutionId
              )
          ]
      )

instance Prelude.ToPath StopNotebookExecution where
  toPath = Prelude.const "/"

instance Prelude.ToQuery StopNotebookExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStopNotebookExecutionResponse' smart constructor.
data StopNotebookExecutionResponse = StopNotebookExecutionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'StopNotebookExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newStopNotebookExecutionResponse ::
  StopNotebookExecutionResponse
newStopNotebookExecutionResponse =
  StopNotebookExecutionResponse'

instance Prelude.NFData StopNotebookExecutionResponse
