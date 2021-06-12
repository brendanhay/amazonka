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
-- Module      : Network.AWS.EMR.StartNotebookExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a notebook execution.
module Network.AWS.EMR.StartNotebookExecution
  ( -- * Creating a Request
    StartNotebookExecution (..),
    newStartNotebookExecution,

    -- * Request Lenses
    startNotebookExecution_notebookExecutionName,
    startNotebookExecution_notebookParams,
    startNotebookExecution_notebookInstanceSecurityGroupId,
    startNotebookExecution_tags,
    startNotebookExecution_editorId,
    startNotebookExecution_relativePath,
    startNotebookExecution_executionEngine,
    startNotebookExecution_serviceRole,

    -- * Destructuring the Response
    StartNotebookExecutionResponse (..),
    newStartNotebookExecutionResponse,

    -- * Response Lenses
    startNotebookExecutionResponse_notebookExecutionId,
    startNotebookExecutionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartNotebookExecution' smart constructor.
data StartNotebookExecution = StartNotebookExecution'
  { -- | An optional name for the notebook execution.
    notebookExecutionName :: Core.Maybe Core.Text,
    -- | Input parameters in JSON format passed to the EMR Notebook at runtime
    -- for execution.
    notebookParams :: Core.Maybe Core.Text,
    -- | The unique identifier of the Amazon EC2 security group to associate with
    -- the EMR Notebook for this notebook execution.
    notebookInstanceSecurityGroupId :: Core.Maybe Core.Text,
    -- | A list of tags associated with a notebook execution. Tags are
    -- user-defined key-value pairs that consist of a required key string with
    -- a maximum of 128 characters and an optional value string with a maximum
    -- of 256 characters.
    tags :: Core.Maybe [Tag],
    -- | The unique identifier of the EMR Notebook to use for notebook execution.
    editorId :: Core.Text,
    -- | The path and file name of the notebook file for this execution, relative
    -- to the path specified for the EMR Notebook. For example, if you specify
    -- a path of @s3:\/\/MyBucket\/MyNotebooks@ when you create an EMR Notebook
    -- for a notebook with an ID of @e-ABCDEFGHIJK1234567890ABCD@ (the
    -- @EditorID@ of this request), and you specify a @RelativePath@ of
    -- @my_notebook_executions\/notebook_execution.ipynb@, the location of the
    -- file for the notebook execution is
    -- @s3:\/\/MyBucket\/MyNotebooks\/e-ABCDEFGHIJK1234567890ABCD\/my_notebook_executions\/notebook_execution.ipynb@.
    relativePath :: Core.Text,
    -- | Specifies the execution engine (cluster) that runs the notebook
    -- execution.
    executionEngine :: ExecutionEngineConfig,
    -- | The name or ARN of the IAM role that is used as the service role for
    -- Amazon EMR (the EMR role) for the notebook execution.
    serviceRole :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartNotebookExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookExecutionName', 'startNotebookExecution_notebookExecutionName' - An optional name for the notebook execution.
--
-- 'notebookParams', 'startNotebookExecution_notebookParams' - Input parameters in JSON format passed to the EMR Notebook at runtime
-- for execution.
--
-- 'notebookInstanceSecurityGroupId', 'startNotebookExecution_notebookInstanceSecurityGroupId' - The unique identifier of the Amazon EC2 security group to associate with
-- the EMR Notebook for this notebook execution.
--
-- 'tags', 'startNotebookExecution_tags' - A list of tags associated with a notebook execution. Tags are
-- user-defined key-value pairs that consist of a required key string with
-- a maximum of 128 characters and an optional value string with a maximum
-- of 256 characters.
--
-- 'editorId', 'startNotebookExecution_editorId' - The unique identifier of the EMR Notebook to use for notebook execution.
--
-- 'relativePath', 'startNotebookExecution_relativePath' - The path and file name of the notebook file for this execution, relative
-- to the path specified for the EMR Notebook. For example, if you specify
-- a path of @s3:\/\/MyBucket\/MyNotebooks@ when you create an EMR Notebook
-- for a notebook with an ID of @e-ABCDEFGHIJK1234567890ABCD@ (the
-- @EditorID@ of this request), and you specify a @RelativePath@ of
-- @my_notebook_executions\/notebook_execution.ipynb@, the location of the
-- file for the notebook execution is
-- @s3:\/\/MyBucket\/MyNotebooks\/e-ABCDEFGHIJK1234567890ABCD\/my_notebook_executions\/notebook_execution.ipynb@.
--
-- 'executionEngine', 'startNotebookExecution_executionEngine' - Specifies the execution engine (cluster) that runs the notebook
-- execution.
--
-- 'serviceRole', 'startNotebookExecution_serviceRole' - The name or ARN of the IAM role that is used as the service role for
-- Amazon EMR (the EMR role) for the notebook execution.
newStartNotebookExecution ::
  -- | 'editorId'
  Core.Text ->
  -- | 'relativePath'
  Core.Text ->
  -- | 'executionEngine'
  ExecutionEngineConfig ->
  -- | 'serviceRole'
  Core.Text ->
  StartNotebookExecution
newStartNotebookExecution
  pEditorId_
  pRelativePath_
  pExecutionEngine_
  pServiceRole_ =
    StartNotebookExecution'
      { notebookExecutionName =
          Core.Nothing,
        notebookParams = Core.Nothing,
        notebookInstanceSecurityGroupId = Core.Nothing,
        tags = Core.Nothing,
        editorId = pEditorId_,
        relativePath = pRelativePath_,
        executionEngine = pExecutionEngine_,
        serviceRole = pServiceRole_
      }

-- | An optional name for the notebook execution.
startNotebookExecution_notebookExecutionName :: Lens.Lens' StartNotebookExecution (Core.Maybe Core.Text)
startNotebookExecution_notebookExecutionName = Lens.lens (\StartNotebookExecution' {notebookExecutionName} -> notebookExecutionName) (\s@StartNotebookExecution' {} a -> s {notebookExecutionName = a} :: StartNotebookExecution)

-- | Input parameters in JSON format passed to the EMR Notebook at runtime
-- for execution.
startNotebookExecution_notebookParams :: Lens.Lens' StartNotebookExecution (Core.Maybe Core.Text)
startNotebookExecution_notebookParams = Lens.lens (\StartNotebookExecution' {notebookParams} -> notebookParams) (\s@StartNotebookExecution' {} a -> s {notebookParams = a} :: StartNotebookExecution)

-- | The unique identifier of the Amazon EC2 security group to associate with
-- the EMR Notebook for this notebook execution.
startNotebookExecution_notebookInstanceSecurityGroupId :: Lens.Lens' StartNotebookExecution (Core.Maybe Core.Text)
startNotebookExecution_notebookInstanceSecurityGroupId = Lens.lens (\StartNotebookExecution' {notebookInstanceSecurityGroupId} -> notebookInstanceSecurityGroupId) (\s@StartNotebookExecution' {} a -> s {notebookInstanceSecurityGroupId = a} :: StartNotebookExecution)

-- | A list of tags associated with a notebook execution. Tags are
-- user-defined key-value pairs that consist of a required key string with
-- a maximum of 128 characters and an optional value string with a maximum
-- of 256 characters.
startNotebookExecution_tags :: Lens.Lens' StartNotebookExecution (Core.Maybe [Tag])
startNotebookExecution_tags = Lens.lens (\StartNotebookExecution' {tags} -> tags) (\s@StartNotebookExecution' {} a -> s {tags = a} :: StartNotebookExecution) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier of the EMR Notebook to use for notebook execution.
startNotebookExecution_editorId :: Lens.Lens' StartNotebookExecution Core.Text
startNotebookExecution_editorId = Lens.lens (\StartNotebookExecution' {editorId} -> editorId) (\s@StartNotebookExecution' {} a -> s {editorId = a} :: StartNotebookExecution)

-- | The path and file name of the notebook file for this execution, relative
-- to the path specified for the EMR Notebook. For example, if you specify
-- a path of @s3:\/\/MyBucket\/MyNotebooks@ when you create an EMR Notebook
-- for a notebook with an ID of @e-ABCDEFGHIJK1234567890ABCD@ (the
-- @EditorID@ of this request), and you specify a @RelativePath@ of
-- @my_notebook_executions\/notebook_execution.ipynb@, the location of the
-- file for the notebook execution is
-- @s3:\/\/MyBucket\/MyNotebooks\/e-ABCDEFGHIJK1234567890ABCD\/my_notebook_executions\/notebook_execution.ipynb@.
startNotebookExecution_relativePath :: Lens.Lens' StartNotebookExecution Core.Text
startNotebookExecution_relativePath = Lens.lens (\StartNotebookExecution' {relativePath} -> relativePath) (\s@StartNotebookExecution' {} a -> s {relativePath = a} :: StartNotebookExecution)

-- | Specifies the execution engine (cluster) that runs the notebook
-- execution.
startNotebookExecution_executionEngine :: Lens.Lens' StartNotebookExecution ExecutionEngineConfig
startNotebookExecution_executionEngine = Lens.lens (\StartNotebookExecution' {executionEngine} -> executionEngine) (\s@StartNotebookExecution' {} a -> s {executionEngine = a} :: StartNotebookExecution)

-- | The name or ARN of the IAM role that is used as the service role for
-- Amazon EMR (the EMR role) for the notebook execution.
startNotebookExecution_serviceRole :: Lens.Lens' StartNotebookExecution Core.Text
startNotebookExecution_serviceRole = Lens.lens (\StartNotebookExecution' {serviceRole} -> serviceRole) (\s@StartNotebookExecution' {} a -> s {serviceRole = a} :: StartNotebookExecution)

instance Core.AWSRequest StartNotebookExecution where
  type
    AWSResponse StartNotebookExecution =
      StartNotebookExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartNotebookExecutionResponse'
            Core.<$> (x Core..?> "NotebookExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartNotebookExecution

instance Core.NFData StartNotebookExecution

instance Core.ToHeaders StartNotebookExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.StartNotebookExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartNotebookExecution where
  toJSON StartNotebookExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NotebookExecutionName" Core..=)
              Core.<$> notebookExecutionName,
            ("NotebookParams" Core..=) Core.<$> notebookParams,
            ("NotebookInstanceSecurityGroupId" Core..=)
              Core.<$> notebookInstanceSecurityGroupId,
            ("Tags" Core..=) Core.<$> tags,
            Core.Just ("EditorId" Core..= editorId),
            Core.Just ("RelativePath" Core..= relativePath),
            Core.Just
              ("ExecutionEngine" Core..= executionEngine),
            Core.Just ("ServiceRole" Core..= serviceRole)
          ]
      )

instance Core.ToPath StartNotebookExecution where
  toPath = Core.const "/"

instance Core.ToQuery StartNotebookExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartNotebookExecutionResponse' smart constructor.
data StartNotebookExecutionResponse = StartNotebookExecutionResponse'
  { -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartNotebookExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookExecutionId', 'startNotebookExecutionResponse_notebookExecutionId' - The unique identifier of the notebook execution.
--
-- 'httpStatus', 'startNotebookExecutionResponse_httpStatus' - The response's http status code.
newStartNotebookExecutionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartNotebookExecutionResponse
newStartNotebookExecutionResponse pHttpStatus_ =
  StartNotebookExecutionResponse'
    { notebookExecutionId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the notebook execution.
startNotebookExecutionResponse_notebookExecutionId :: Lens.Lens' StartNotebookExecutionResponse (Core.Maybe Core.Text)
startNotebookExecutionResponse_notebookExecutionId = Lens.lens (\StartNotebookExecutionResponse' {notebookExecutionId} -> notebookExecutionId) (\s@StartNotebookExecutionResponse' {} a -> s {notebookExecutionId = a} :: StartNotebookExecutionResponse)

-- | The response's http status code.
startNotebookExecutionResponse_httpStatus :: Lens.Lens' StartNotebookExecutionResponse Core.Int
startNotebookExecutionResponse_httpStatus = Lens.lens (\StartNotebookExecutionResponse' {httpStatus} -> httpStatus) (\s@StartNotebookExecutionResponse' {} a -> s {httpStatus = a} :: StartNotebookExecutionResponse)

instance Core.NFData StartNotebookExecutionResponse
