{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.NotebookExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.NotebookExecution
  ( NotebookExecution (..)
  -- * Smart constructor
  , mkNotebookExecution
  -- * Lenses
  , neArn
  , neEditorId
  , neEndTime
  , neExecutionEngine
  , neLastStateChangeReason
  , neNotebookExecutionId
  , neNotebookExecutionName
  , neNotebookInstanceSecurityGroupId
  , neNotebookParams
  , neOutputNotebookURI
  , neStartTime
  , neStatus
  , neTags
  ) where

import qualified Network.AWS.EMR.Types.Arn as Types
import qualified Network.AWS.EMR.Types.EditorId as Types
import qualified Network.AWS.EMR.Types.ExecutionEngineConfig as Types
import qualified Network.AWS.EMR.Types.NotebookExecutionId as Types
import qualified Network.AWS.EMR.Types.NotebookExecutionName as Types
import qualified Network.AWS.EMR.Types.NotebookExecutionStatus as Types
import qualified Network.AWS.EMR.Types.NotebookInstanceSecurityGroupId as Types
import qualified Network.AWS.EMR.Types.Tag as Types
import qualified Network.AWS.EMR.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A notebook execution. An execution is a specific instance that an EMR Notebook is run using the @StartNotebookExecution@ action.
--
-- /See:/ 'mkNotebookExecution' smart constructor.
data NotebookExecution = NotebookExecution'
  { arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the notebook execution.
  , editorId :: Core.Maybe Types.EditorId
    -- ^ The unique identifier of the EMR Notebook that is used for the notebook execution.
  , endTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when notebook execution ended.
  , executionEngine :: Core.Maybe Types.ExecutionEngineConfig
    -- ^ The execution engine, such as an EMR cluster, used to run the EMR notebook and perform the notebook execution.
  , lastStateChangeReason :: Core.Maybe Types.XmlString
    -- ^ The reason for the latest status change of the notebook execution.
  , notebookExecutionId :: Core.Maybe Types.NotebookExecutionId
    -- ^ The unique identifier of a notebook execution.
  , notebookExecutionName :: Core.Maybe Types.NotebookExecutionName
    -- ^ A name for the notebook execution.
  , notebookInstanceSecurityGroupId :: Core.Maybe Types.NotebookInstanceSecurityGroupId
    -- ^ The unique identifier of the EC2 security group associated with the EMR Notebook instance. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks> in the /EMR Management Guide/ .
  , notebookParams :: Core.Maybe Types.XmlString
    -- ^ Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
  , outputNotebookURI :: Core.Maybe Types.XmlString
    -- ^ The location of the notebook execution's output file in Amazon S3.
  , startTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The timestamp when notebook execution started.
  , status :: Core.Maybe Types.NotebookExecutionStatus
    -- ^ The status of the notebook execution.
--
--
--     * @START_PENDING@ indicates that the cluster has received the execution request but execution has not begun.
--
--
--     * @STARTING@ indicates that the execution is starting on the cluster.
--
--
--     * @RUNNING@ indicates that the execution is being processed by the cluster.
--
--
--     * @FINISHING@ indicates that execution processing is in the final stages.
--
--
--     * @FINISHED@ indicates that the execution has completed without error.
--
--
--     * @FAILING@ indicates that the execution is failing and will not finish successfully.
--
--
--     * @FAILED@ indicates that the execution failed.
--
--
--     * @STOP_PENDING@ indicates that the cluster has received a @StopNotebookExecution@ request and the stop is pending.
--
--
--     * @STOPPING@ indicates that the cluster is in the process of stopping the execution as a result of a @StopNotebookExecution@ request.
--
--
--     * @STOPPED@ indicates that the execution stopped because of a @StopNotebookExecution@ request.
--
--
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'NotebookExecution' value with any optional fields omitted.
mkNotebookExecution
    :: NotebookExecution
mkNotebookExecution
  = NotebookExecution'{arn = Core.Nothing, editorId = Core.Nothing,
                       endTime = Core.Nothing, executionEngine = Core.Nothing,
                       lastStateChangeReason = Core.Nothing,
                       notebookExecutionId = Core.Nothing,
                       notebookExecutionName = Core.Nothing,
                       notebookInstanceSecurityGroupId = Core.Nothing,
                       notebookParams = Core.Nothing, outputNotebookURI = Core.Nothing,
                       startTime = Core.Nothing, status = Core.Nothing,
                       tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the notebook execution.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neArn :: Lens.Lens' NotebookExecution (Core.Maybe Types.Arn)
neArn = Lens.field @"arn"
{-# INLINEABLE neArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The unique identifier of the EMR Notebook that is used for the notebook execution.
--
-- /Note:/ Consider using 'editorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neEditorId :: Lens.Lens' NotebookExecution (Core.Maybe Types.EditorId)
neEditorId = Lens.field @"editorId"
{-# INLINEABLE neEditorId #-}
{-# DEPRECATED editorId "Use generic-lens or generic-optics with 'editorId' instead"  #-}

-- | The timestamp when notebook execution ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neEndTime :: Lens.Lens' NotebookExecution (Core.Maybe Core.NominalDiffTime)
neEndTime = Lens.field @"endTime"
{-# INLINEABLE neEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The execution engine, such as an EMR cluster, used to run the EMR notebook and perform the notebook execution.
--
-- /Note:/ Consider using 'executionEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neExecutionEngine :: Lens.Lens' NotebookExecution (Core.Maybe Types.ExecutionEngineConfig)
neExecutionEngine = Lens.field @"executionEngine"
{-# INLINEABLE neExecutionEngine #-}
{-# DEPRECATED executionEngine "Use generic-lens or generic-optics with 'executionEngine' instead"  #-}

-- | The reason for the latest status change of the notebook execution.
--
-- /Note:/ Consider using 'lastStateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neLastStateChangeReason :: Lens.Lens' NotebookExecution (Core.Maybe Types.XmlString)
neLastStateChangeReason = Lens.field @"lastStateChangeReason"
{-# INLINEABLE neLastStateChangeReason #-}
{-# DEPRECATED lastStateChangeReason "Use generic-lens or generic-optics with 'lastStateChangeReason' instead"  #-}

-- | The unique identifier of a notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neNotebookExecutionId :: Lens.Lens' NotebookExecution (Core.Maybe Types.NotebookExecutionId)
neNotebookExecutionId = Lens.field @"notebookExecutionId"
{-# INLINEABLE neNotebookExecutionId #-}
{-# DEPRECATED notebookExecutionId "Use generic-lens or generic-optics with 'notebookExecutionId' instead"  #-}

-- | A name for the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neNotebookExecutionName :: Lens.Lens' NotebookExecution (Core.Maybe Types.NotebookExecutionName)
neNotebookExecutionName = Lens.field @"notebookExecutionName"
{-# INLINEABLE neNotebookExecutionName #-}
{-# DEPRECATED notebookExecutionName "Use generic-lens or generic-optics with 'notebookExecutionName' instead"  #-}

-- | The unique identifier of the EC2 security group associated with the EMR Notebook instance. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks> in the /EMR Management Guide/ .
--
-- /Note:/ Consider using 'notebookInstanceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neNotebookInstanceSecurityGroupId :: Lens.Lens' NotebookExecution (Core.Maybe Types.NotebookInstanceSecurityGroupId)
neNotebookInstanceSecurityGroupId = Lens.field @"notebookInstanceSecurityGroupId"
{-# INLINEABLE neNotebookInstanceSecurityGroupId #-}
{-# DEPRECATED notebookInstanceSecurityGroupId "Use generic-lens or generic-optics with 'notebookInstanceSecurityGroupId' instead"  #-}

-- | Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
--
-- /Note:/ Consider using 'notebookParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neNotebookParams :: Lens.Lens' NotebookExecution (Core.Maybe Types.XmlString)
neNotebookParams = Lens.field @"notebookParams"
{-# INLINEABLE neNotebookParams #-}
{-# DEPRECATED notebookParams "Use generic-lens or generic-optics with 'notebookParams' instead"  #-}

-- | The location of the notebook execution's output file in Amazon S3.
--
-- /Note:/ Consider using 'outputNotebookURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neOutputNotebookURI :: Lens.Lens' NotebookExecution (Core.Maybe Types.XmlString)
neOutputNotebookURI = Lens.field @"outputNotebookURI"
{-# INLINEABLE neOutputNotebookURI #-}
{-# DEPRECATED outputNotebookURI "Use generic-lens or generic-optics with 'outputNotebookURI' instead"  #-}

-- | The timestamp when notebook execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neStartTime :: Lens.Lens' NotebookExecution (Core.Maybe Core.NominalDiffTime)
neStartTime = Lens.field @"startTime"
{-# INLINEABLE neStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The status of the notebook execution.
--
--
--     * @START_PENDING@ indicates that the cluster has received the execution request but execution has not begun.
--
--
--     * @STARTING@ indicates that the execution is starting on the cluster.
--
--
--     * @RUNNING@ indicates that the execution is being processed by the cluster.
--
--
--     * @FINISHING@ indicates that execution processing is in the final stages.
--
--
--     * @FINISHED@ indicates that the execution has completed without error.
--
--
--     * @FAILING@ indicates that the execution is failing and will not finish successfully.
--
--
--     * @FAILED@ indicates that the execution failed.
--
--
--     * @STOP_PENDING@ indicates that the cluster has received a @StopNotebookExecution@ request and the stop is pending.
--
--
--     * @STOPPING@ indicates that the cluster is in the process of stopping the execution as a result of a @StopNotebookExecution@ request.
--
--
--     * @STOPPED@ indicates that the execution stopped because of a @StopNotebookExecution@ request.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neStatus :: Lens.Lens' NotebookExecution (Core.Maybe Types.NotebookExecutionStatus)
neStatus = Lens.field @"status"
{-# INLINEABLE neStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neTags :: Lens.Lens' NotebookExecution (Core.Maybe [Types.Tag])
neTags = Lens.field @"tags"
{-# INLINEABLE neTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON NotebookExecution where
        parseJSON
          = Core.withObject "NotebookExecution" Core.$
              \ x ->
                NotebookExecution' Core.<$>
                  (x Core..:? "Arn") Core.<*> x Core..:? "EditorId" Core.<*>
                    x Core..:? "EndTime"
                    Core.<*> x Core..:? "ExecutionEngine"
                    Core.<*> x Core..:? "LastStateChangeReason"
                    Core.<*> x Core..:? "NotebookExecutionId"
                    Core.<*> x Core..:? "NotebookExecutionName"
                    Core.<*> x Core..:? "NotebookInstanceSecurityGroupId"
                    Core.<*> x Core..:? "NotebookParams"
                    Core.<*> x Core..:? "OutputNotebookURI"
                    Core.<*> x Core..:? "StartTime"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "Tags"
