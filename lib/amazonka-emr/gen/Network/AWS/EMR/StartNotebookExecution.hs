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
    sneEditorId,
    sneRelativePath,
    sneExecutionEngine,
    sneServiceRole,
    sneNotebookExecutionName,
    sneNotebookInstanceSecurityGroupId,
    sneNotebookParams,
    sneTags,

    -- * Destructuring the response
    StartNotebookExecutionResponse (..),
    mkStartNotebookExecutionResponse,

    -- ** Response lenses
    snerrsNotebookExecutionId,
    snerrsResponseStatus,
  )
where

import qualified Network.AWS.EMR.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartNotebookExecution' smart constructor.
data StartNotebookExecution = StartNotebookExecution'
  { -- | The unique identifier of the EMR Notebook to use for notebook execution.
    editorId :: Types.EditorId,
    -- | The path and file name of the notebook file for this execution, relative to the path specified for the EMR Notebook. For example, if you specify a path of @s3://MyBucket/MyNotebooks@ when you create an EMR Notebook for a notebook with an ID of @e-ABCDEFGHIJK1234567890ABCD@ (the @EditorID@ of this request), and you specify a @RelativePath@ of @my_notebook_executions/notebook_execution.ipynb@ , the location of the file for the notebook execution is @s3://MyBucket/MyNotebooks/e-ABCDEFGHIJK1234567890ABCD/my_notebook_executions/notebook_execution.ipynb@ .
    relativePath :: Types.XmlString,
    -- | Specifies the execution engine (cluster) that runs the notebook execution.
    executionEngine :: Types.ExecutionEngineConfig,
    -- | The name or ARN of the IAM role that is used as the service role for Amazon EMR (the EMR role) for the notebook execution.
    serviceRole :: Types.XmlString,
    -- | An optional name for the notebook execution.
    notebookExecutionName :: Core.Maybe Types.NotebookExecutionName,
    -- | The unique identifier of the Amazon EC2 security group to associate with the EMR Notebook for this notebook execution.
    notebookInstanceSecurityGroupId :: Core.Maybe Types.NotebookInstanceSecurityGroupId,
    -- | Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
    notebookParams :: Core.Maybe Types.XmlString,
    -- | A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartNotebookExecution' value with any optional fields omitted.
mkStartNotebookExecution ::
  -- | 'editorId'
  Types.EditorId ->
  -- | 'relativePath'
  Types.XmlString ->
  -- | 'executionEngine'
  Types.ExecutionEngineConfig ->
  -- | 'serviceRole'
  Types.XmlString ->
  StartNotebookExecution
mkStartNotebookExecution
  editorId
  relativePath
  executionEngine
  serviceRole =
    StartNotebookExecution'
      { editorId,
        relativePath,
        executionEngine,
        serviceRole,
        notebookExecutionName = Core.Nothing,
        notebookInstanceSecurityGroupId = Core.Nothing,
        notebookParams = Core.Nothing,
        tags = Core.Nothing
      }

-- | The unique identifier of the EMR Notebook to use for notebook execution.
--
-- /Note:/ Consider using 'editorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneEditorId :: Lens.Lens' StartNotebookExecution Types.EditorId
sneEditorId = Lens.field @"editorId"
{-# DEPRECATED sneEditorId "Use generic-lens or generic-optics with 'editorId' instead." #-}

-- | The path and file name of the notebook file for this execution, relative to the path specified for the EMR Notebook. For example, if you specify a path of @s3://MyBucket/MyNotebooks@ when you create an EMR Notebook for a notebook with an ID of @e-ABCDEFGHIJK1234567890ABCD@ (the @EditorID@ of this request), and you specify a @RelativePath@ of @my_notebook_executions/notebook_execution.ipynb@ , the location of the file for the notebook execution is @s3://MyBucket/MyNotebooks/e-ABCDEFGHIJK1234567890ABCD/my_notebook_executions/notebook_execution.ipynb@ .
--
-- /Note:/ Consider using 'relativePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneRelativePath :: Lens.Lens' StartNotebookExecution Types.XmlString
sneRelativePath = Lens.field @"relativePath"
{-# DEPRECATED sneRelativePath "Use generic-lens or generic-optics with 'relativePath' instead." #-}

-- | Specifies the execution engine (cluster) that runs the notebook execution.
--
-- /Note:/ Consider using 'executionEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneExecutionEngine :: Lens.Lens' StartNotebookExecution Types.ExecutionEngineConfig
sneExecutionEngine = Lens.field @"executionEngine"
{-# DEPRECATED sneExecutionEngine "Use generic-lens or generic-optics with 'executionEngine' instead." #-}

-- | The name or ARN of the IAM role that is used as the service role for Amazon EMR (the EMR role) for the notebook execution.
--
-- /Note:/ Consider using 'serviceRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneServiceRole :: Lens.Lens' StartNotebookExecution Types.XmlString
sneServiceRole = Lens.field @"serviceRole"
{-# DEPRECATED sneServiceRole "Use generic-lens or generic-optics with 'serviceRole' instead." #-}

-- | An optional name for the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneNotebookExecutionName :: Lens.Lens' StartNotebookExecution (Core.Maybe Types.NotebookExecutionName)
sneNotebookExecutionName = Lens.field @"notebookExecutionName"
{-# DEPRECATED sneNotebookExecutionName "Use generic-lens or generic-optics with 'notebookExecutionName' instead." #-}

-- | The unique identifier of the Amazon EC2 security group to associate with the EMR Notebook for this notebook execution.
--
-- /Note:/ Consider using 'notebookInstanceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneNotebookInstanceSecurityGroupId :: Lens.Lens' StartNotebookExecution (Core.Maybe Types.NotebookInstanceSecurityGroupId)
sneNotebookInstanceSecurityGroupId = Lens.field @"notebookInstanceSecurityGroupId"
{-# DEPRECATED sneNotebookInstanceSecurityGroupId "Use generic-lens or generic-optics with 'notebookInstanceSecurityGroupId' instead." #-}

-- | Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
--
-- /Note:/ Consider using 'notebookParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneNotebookParams :: Lens.Lens' StartNotebookExecution (Core.Maybe Types.XmlString)
sneNotebookParams = Lens.field @"notebookParams"
{-# DEPRECATED sneNotebookParams "Use generic-lens or generic-optics with 'notebookParams' instead." #-}

-- | A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sneTags :: Lens.Lens' StartNotebookExecution (Core.Maybe [Types.Tag])
sneTags = Lens.field @"tags"
{-# DEPRECATED sneTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON StartNotebookExecution where
  toJSON StartNotebookExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EditorId" Core..= editorId),
            Core.Just ("RelativePath" Core..= relativePath),
            Core.Just ("ExecutionEngine" Core..= executionEngine),
            Core.Just ("ServiceRole" Core..= serviceRole),
            ("NotebookExecutionName" Core..=) Core.<$> notebookExecutionName,
            ("NotebookInstanceSecurityGroupId" Core..=)
              Core.<$> notebookInstanceSecurityGroupId,
            ("NotebookParams" Core..=) Core.<$> notebookParams,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest StartNotebookExecution where
  type Rs StartNotebookExecution = StartNotebookExecutionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "ElasticMapReduce.StartNotebookExecution")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          StartNotebookExecutionResponse'
            Core.<$> (x Core..:? "NotebookExecutionId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartNotebookExecutionResponse' smart constructor.
data StartNotebookExecutionResponse = StartNotebookExecutionResponse'
  { -- | The unique identifier of the notebook execution.
    notebookExecutionId :: Core.Maybe Types.XmlStringMaxLen256,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StartNotebookExecutionResponse' value with any optional fields omitted.
mkStartNotebookExecutionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartNotebookExecutionResponse
mkStartNotebookExecutionResponse responseStatus =
  StartNotebookExecutionResponse'
    { notebookExecutionId =
        Core.Nothing,
      responseStatus
    }

-- | The unique identifier of the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snerrsNotebookExecutionId :: Lens.Lens' StartNotebookExecutionResponse (Core.Maybe Types.XmlStringMaxLen256)
snerrsNotebookExecutionId = Lens.field @"notebookExecutionId"
{-# DEPRECATED snerrsNotebookExecutionId "Use generic-lens or generic-optics with 'notebookExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snerrsResponseStatus :: Lens.Lens' StartNotebookExecutionResponse Core.Int
snerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED snerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
