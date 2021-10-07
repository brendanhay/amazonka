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
-- Module      : Network.AWS.EMR.DescribeNotebookExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details of a notebook execution.
module Network.AWS.EMR.DescribeNotebookExecution
  ( -- * Creating a Request
    DescribeNotebookExecution (..),
    newDescribeNotebookExecution,

    -- * Request Lenses
    describeNotebookExecution_notebookExecutionId,

    -- * Destructuring the Response
    DescribeNotebookExecutionResponse (..),
    newDescribeNotebookExecutionResponse,

    -- * Response Lenses
    describeNotebookExecutionResponse_notebookExecution,
    describeNotebookExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeNotebookExecution' smart constructor.
data DescribeNotebookExecution = DescribeNotebookExecution'
  { -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNotebookExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookExecutionId', 'describeNotebookExecution_notebookExecutionId' - The unique identifier of the notebook execution.
newDescribeNotebookExecution ::
  -- | 'notebookExecutionId'
  Prelude.Text ->
  DescribeNotebookExecution
newDescribeNotebookExecution pNotebookExecutionId_ =
  DescribeNotebookExecution'
    { notebookExecutionId =
        pNotebookExecutionId_
    }

-- | The unique identifier of the notebook execution.
describeNotebookExecution_notebookExecutionId :: Lens.Lens' DescribeNotebookExecution Prelude.Text
describeNotebookExecution_notebookExecutionId = Lens.lens (\DescribeNotebookExecution' {notebookExecutionId} -> notebookExecutionId) (\s@DescribeNotebookExecution' {} a -> s {notebookExecutionId = a} :: DescribeNotebookExecution)

instance Core.AWSRequest DescribeNotebookExecution where
  type
    AWSResponse DescribeNotebookExecution =
      DescribeNotebookExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotebookExecutionResponse'
            Prelude.<$> (x Core..?> "NotebookExecution")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeNotebookExecution

instance Prelude.NFData DescribeNotebookExecution

instance Core.ToHeaders DescribeNotebookExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.DescribeNotebookExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeNotebookExecution where
  toJSON DescribeNotebookExecution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("NotebookExecutionId" Core..= notebookExecutionId)
          ]
      )

instance Core.ToPath DescribeNotebookExecution where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeNotebookExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeNotebookExecutionResponse' smart constructor.
data DescribeNotebookExecutionResponse = DescribeNotebookExecutionResponse'
  { -- | Properties of the notebook execution.
    notebookExecution :: Prelude.Maybe NotebookExecution,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNotebookExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookExecution', 'describeNotebookExecutionResponse_notebookExecution' - Properties of the notebook execution.
--
-- 'httpStatus', 'describeNotebookExecutionResponse_httpStatus' - The response's http status code.
newDescribeNotebookExecutionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeNotebookExecutionResponse
newDescribeNotebookExecutionResponse pHttpStatus_ =
  DescribeNotebookExecutionResponse'
    { notebookExecution =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Properties of the notebook execution.
describeNotebookExecutionResponse_notebookExecution :: Lens.Lens' DescribeNotebookExecutionResponse (Prelude.Maybe NotebookExecution)
describeNotebookExecutionResponse_notebookExecution = Lens.lens (\DescribeNotebookExecutionResponse' {notebookExecution} -> notebookExecution) (\s@DescribeNotebookExecutionResponse' {} a -> s {notebookExecution = a} :: DescribeNotebookExecutionResponse)

-- | The response's http status code.
describeNotebookExecutionResponse_httpStatus :: Lens.Lens' DescribeNotebookExecutionResponse Prelude.Int
describeNotebookExecutionResponse_httpStatus = Lens.lens (\DescribeNotebookExecutionResponse' {httpStatus} -> httpStatus) (\s@DescribeNotebookExecutionResponse' {} a -> s {httpStatus = a} :: DescribeNotebookExecutionResponse)

instance
  Prelude.NFData
    DescribeNotebookExecutionResponse
