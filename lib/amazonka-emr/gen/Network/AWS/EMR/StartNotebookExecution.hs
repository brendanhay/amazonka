{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.StartNotebookExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts a notebook execution.
module Network.AWS.EMR.StartNotebookExecution
  ( -- * Creating a request
    StartNotebookExecution (..),
    mkStartNotebookExecution,

    -- ** Request lenses
    sneExecutionEngine,
    sneNotebookInstanceSecurityGroupId,
    sneEditorId,
    sneNotebookExecutionName,
    sneNotebookParams,
    sneTags,
    sneRelativePath,
    sneServiceRole,

    -- * Destructuring the response
    StartNotebookExecutionResponse (..),
    mkStartNotebookExecutionResponse,

    -- ** Response lenses
    snersNotebookExecutionId,
    snersResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartNotebookExecution' smart constructor.
data StartNotebookExecution = StartNotebookExecution'
  { -- | Specifies the execution engine (cluster) that runs the notebook execution.
    executionEngine :: ExecutionEngineConfig,
    -- | The unique identifier of the Amazon EC2 security group to associate with the EMR Notebook for this notebook execution.
    notebookInstanceSecurityGroupId :: Lude.Maybe Lude.Text,
    -- | The unique identifier of the EMR Notebook to use for notebook execution.
    editorId :: Lude.Text,
    -- | An optional name for the notebook execution.
    notebookExecutionName :: Lude.Maybe Lude.Text,
    -- | Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
    notebookParams :: Lude.Maybe Lude.Text,
    -- | A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
    tags :: Lude.Maybe [Tag],
    -- | The path and file name of the notebook file for this execution, relative to the path specified for the EMR Notebook. For example, if you specify a path of @s3://MyBucket/MyNotebooks@ when you create an EMR Notebook for a notebook with an ID of @e-ABCDEFGHIJK1234567890ABCD@ (the @EditorID@ of this request), and you specify a @RelativePath@ of @my_notebook_executions/notebook_execution.ipynb@ , the location of the file for the notebook execution is @s3://MyBucket/MyNotebooks/e-ABCDEFGHIJK1234567890ABCD/my_notebook_executions/notebook_execution.ipynb@ .
    relativePath :: Lude.Text,
    -- | The name or ARN of the IAM role that is used as the service role for Amazon EMR (the EMR role) for the notebook execution.
    serviceRole :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartNotebookExecution' with the minimum fields required to make a request.
--
-- * 'executionEngine' - Specifies the execution engine (cluster) that runs the notebook execution.
-- * 'notebookInstanceSecurityGroupId' - The unique identifier of the Amazon EC2 security group to associate with the EMR Notebook for this notebook execution.
-- * 'editorId' - The unique identifier of the EMR Notebook to use for notebook execution.
-- * 'notebookExecutionName' - An optional name for the notebook execution.
-- * 'notebookParams' - Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
-- * 'tags' - A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
-- * 'relativePath' - The path and file name of the notebook file for this execution, relative to the path specified for the EMR Notebook. For example, if you specify a path of @s3://MyBucket/MyNotebooks@ when you create an EMR Notebook for a notebook with an ID of @e-ABCDEFGHIJK1234567890ABCD@ (the @EditorID@ of this request), and you specify a @RelativePath@ of @my_notebook_executions/notebook_execution.ipynb@ , the location of the file for the notebook execution is @s3://MyBucket/MyNotebooks/e-ABCDEFGHIJK1234567890ABCD/my_notebook_executions/notebook_execution.ipynb@ .
-- * 'serviceRole' - The name or ARN of the IAM role that is used as the service role for Amazon EMR (the EMR role) for the notebook execution.
mkStartNotebookExecution ::
  -- | 'executionEngine'
  ExecutionEngineConfig ->
  -- | 'editorId'
  Lude.Text ->
  -- | 'relativePath'
  Lude.Text ->
  -- | 'serviceRole'
  Lude.Text ->
  StartNotebookExecution
mkStartNotebookExecution
  pExecutionEngine_
  pEditorId_
  pRelativePath_
  pServiceRole_ =
    StartNotebookExecution'
      { executionEngine = pExecutionEngine_,
        notebookInstanceSecurityGroupId = Lude.Nothing,
        editorId = pEditorId_,
        notebookExecutionName = Lude.Nothing,
        notebookParams = Lude.Nothing,
        tags = Lude.Nothing,
        relativePath = pRelativePath_,
        serviceRole = pServiceRole_
      }

-- | Specifies the execution engine (cluster) that runs the notebook execution.
--
-- /Note:/ Consider using 'executionEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneExecutionEngine :: Lens.Lens' StartNotebookExecution ExecutionEngineConfig
sneExecutionEngine = Lens.lens (executionEngine :: StartNotebookExecution -> ExecutionEngineConfig) (\s a -> s {executionEngine = a} :: StartNotebookExecution)
{-# DEPRECATED sneExecutionEngine "Use generic-lens or generic-optics with 'executionEngine' instead." #-}

-- | The unique identifier of the Amazon EC2 security group to associate with the EMR Notebook for this notebook execution.
--
-- /Note:/ Consider using 'notebookInstanceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneNotebookInstanceSecurityGroupId :: Lens.Lens' StartNotebookExecution (Lude.Maybe Lude.Text)
sneNotebookInstanceSecurityGroupId = Lens.lens (notebookInstanceSecurityGroupId :: StartNotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceSecurityGroupId = a} :: StartNotebookExecution)
{-# DEPRECATED sneNotebookInstanceSecurityGroupId "Use generic-lens or generic-optics with 'notebookInstanceSecurityGroupId' instead." #-}

-- | The unique identifier of the EMR Notebook to use for notebook execution.
--
-- /Note:/ Consider using 'editorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneEditorId :: Lens.Lens' StartNotebookExecution Lude.Text
sneEditorId = Lens.lens (editorId :: StartNotebookExecution -> Lude.Text) (\s a -> s {editorId = a} :: StartNotebookExecution)
{-# DEPRECATED sneEditorId "Use generic-lens or generic-optics with 'editorId' instead." #-}

-- | An optional name for the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneNotebookExecutionName :: Lens.Lens' StartNotebookExecution (Lude.Maybe Lude.Text)
sneNotebookExecutionName = Lens.lens (notebookExecutionName :: StartNotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {notebookExecutionName = a} :: StartNotebookExecution)
{-# DEPRECATED sneNotebookExecutionName "Use generic-lens or generic-optics with 'notebookExecutionName' instead." #-}

-- | Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
--
-- /Note:/ Consider using 'notebookParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneNotebookParams :: Lens.Lens' StartNotebookExecution (Lude.Maybe Lude.Text)
sneNotebookParams = Lens.lens (notebookParams :: StartNotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {notebookParams = a} :: StartNotebookExecution)
{-# DEPRECATED sneNotebookParams "Use generic-lens or generic-optics with 'notebookParams' instead." #-}

-- | A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneTags :: Lens.Lens' StartNotebookExecution (Lude.Maybe [Tag])
sneTags = Lens.lens (tags :: StartNotebookExecution -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: StartNotebookExecution)
{-# DEPRECATED sneTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The path and file name of the notebook file for this execution, relative to the path specified for the EMR Notebook. For example, if you specify a path of @s3://MyBucket/MyNotebooks@ when you create an EMR Notebook for a notebook with an ID of @e-ABCDEFGHIJK1234567890ABCD@ (the @EditorID@ of this request), and you specify a @RelativePath@ of @my_notebook_executions/notebook_execution.ipynb@ , the location of the file for the notebook execution is @s3://MyBucket/MyNotebooks/e-ABCDEFGHIJK1234567890ABCD/my_notebook_executions/notebook_execution.ipynb@ .
--
-- /Note:/ Consider using 'relativePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneRelativePath :: Lens.Lens' StartNotebookExecution Lude.Text
sneRelativePath = Lens.lens (relativePath :: StartNotebookExecution -> Lude.Text) (\s a -> s {relativePath = a} :: StartNotebookExecution)
{-# DEPRECATED sneRelativePath "Use generic-lens or generic-optics with 'relativePath' instead." #-}

-- | The name or ARN of the IAM role that is used as the service role for Amazon EMR (the EMR role) for the notebook execution.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneServiceRole :: Lens.Lens' StartNotebookExecution Lude.Text
sneServiceRole = Lens.lens (serviceRole :: StartNotebookExecution -> Lude.Text) (\s a -> s {serviceRole = a} :: StartNotebookExecution)
{-# DEPRECATED sneServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

instance Lude.AWSRequest StartNotebookExecution where
  type Rs StartNotebookExecution = StartNotebookExecutionResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartNotebookExecutionResponse'
            Lude.<$> (x Lude..?> "NotebookExecutionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartNotebookExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.StartNotebookExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartNotebookExecution where
  toJSON StartNotebookExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ExecutionEngine" Lude..= executionEngine),
            ("NotebookInstanceSecurityGroupId" Lude..=)
              Lude.<$> notebookInstanceSecurityGroupId,
            Lude.Just ("EditorId" Lude..= editorId),
            ("NotebookExecutionName" Lude..=) Lude.<$> notebookExecutionName,
            ("NotebookParams" Lude..=) Lude.<$> notebookParams,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RelativePath" Lude..= relativePath),
            Lude.Just ("ServiceRole" Lude..= serviceRole)
          ]
      )

instance Lude.ToPath StartNotebookExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StartNotebookExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartNotebookExecutionResponse' smart constructor.
data StartNotebookExecutionResponse = StartNotebookExecutionResponse'
  { -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartNotebookExecutionResponse' with the minimum fields required to make a request.
--
-- * 'notebookExecutionId' - The unique identifier of the notebook execution.
-- * 'responseStatus' - The response status code.
mkStartNotebookExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartNotebookExecutionResponse
mkStartNotebookExecutionResponse pResponseStatus_ =
  StartNotebookExecutionResponse'
    { notebookExecutionId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique identifier of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snersNotebookExecutionId :: Lens.Lens' StartNotebookExecutionResponse (Lude.Maybe Lude.Text)
snersNotebookExecutionId = Lens.lens (notebookExecutionId :: StartNotebookExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {notebookExecutionId = a} :: StartNotebookExecutionResponse)
{-# DEPRECATED snersNotebookExecutionId "Use generic-lens or generic-optics with 'notebookExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snersResponseStatus :: Lens.Lens' StartNotebookExecutionResponse Lude.Int
snersResponseStatus = Lens.lens (responseStatus :: StartNotebookExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartNotebookExecutionResponse)
{-# DEPRECATED snersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
