-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.NotebookExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.NotebookExecution
  ( NotebookExecution (..),

    -- * Smart constructor
    mkNotebookExecution,

    -- * Lenses
    neStatus,
    neExecutionEngine,
    neNotebookInstanceSecurityGroupId,
    neEditorId,
    neStartTime,
    neARN,
    neOutputNotebookURI,
    neNotebookExecutionId,
    neNotebookExecutionName,
    neLastStateChangeReason,
    neEndTime,
    neNotebookParams,
    neTags,
  )
where

import Network.AWS.EMR.Types.ExecutionEngineConfig
import Network.AWS.EMR.Types.NotebookExecutionStatus
import Network.AWS.EMR.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A notebook execution. An execution is a specific instance that an EMR Notebook is run using the @StartNotebookExecution@ action.
--
-- /See:/ 'mkNotebookExecution' smart constructor.
data NotebookExecution = NotebookExecution'
  { status ::
      Lude.Maybe NotebookExecutionStatus,
    executionEngine :: Lude.Maybe ExecutionEngineConfig,
    notebookInstanceSecurityGroupId :: Lude.Maybe Lude.Text,
    editorId :: Lude.Maybe Lude.Text,
    startTime :: Lude.Maybe Lude.Timestamp,
    arn :: Lude.Maybe Lude.Text,
    outputNotebookURI :: Lude.Maybe Lude.Text,
    notebookExecutionId :: Lude.Maybe Lude.Text,
    notebookExecutionName :: Lude.Maybe Lude.Text,
    lastStateChangeReason :: Lude.Maybe Lude.Text,
    endTime :: Lude.Maybe Lude.Timestamp,
    notebookParams :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotebookExecution' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the notebook execution.
-- * 'editorId' - The unique identifier of the EMR Notebook that is used for the notebook execution.
-- * 'endTime' - The timestamp when notebook execution ended.
-- * 'executionEngine' - The execution engine, such as an EMR cluster, used to run the EMR notebook and perform the notebook execution.
-- * 'lastStateChangeReason' - The reason for the latest status change of the notebook execution.
-- * 'notebookExecutionId' - The unique identifier of a notebook execution.
-- * 'notebookExecutionName' - A name for the notebook execution.
-- * 'notebookInstanceSecurityGroupId' - The unique identifier of the EC2 security group associated with the EMR Notebook instance. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks> in the /EMR Management Guide/ .
-- * 'notebookParams' - Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
-- * 'outputNotebookURI' - The location of the notebook execution's output file in Amazon S3.
-- * 'startTime' - The timestamp when notebook execution started.
-- * 'status' - The status of the notebook execution.
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
-- * 'tags' - A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
mkNotebookExecution ::
  NotebookExecution
mkNotebookExecution =
  NotebookExecution'
    { status = Lude.Nothing,
      executionEngine = Lude.Nothing,
      notebookInstanceSecurityGroupId = Lude.Nothing,
      editorId = Lude.Nothing,
      startTime = Lude.Nothing,
      arn = Lude.Nothing,
      outputNotebookURI = Lude.Nothing,
      notebookExecutionId = Lude.Nothing,
      notebookExecutionName = Lude.Nothing,
      lastStateChangeReason = Lude.Nothing,
      endTime = Lude.Nothing,
      notebookParams = Lude.Nothing,
      tags = Lude.Nothing
    }

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
neStatus :: Lens.Lens' NotebookExecution (Lude.Maybe NotebookExecutionStatus)
neStatus = Lens.lens (status :: NotebookExecution -> Lude.Maybe NotebookExecutionStatus) (\s a -> s {status = a} :: NotebookExecution)
{-# DEPRECATED neStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The execution engine, such as an EMR cluster, used to run the EMR notebook and perform the notebook execution.
--
-- /Note:/ Consider using 'executionEngine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neExecutionEngine :: Lens.Lens' NotebookExecution (Lude.Maybe ExecutionEngineConfig)
neExecutionEngine = Lens.lens (executionEngine :: NotebookExecution -> Lude.Maybe ExecutionEngineConfig) (\s a -> s {executionEngine = a} :: NotebookExecution)
{-# DEPRECATED neExecutionEngine "Use generic-lens or generic-optics with 'executionEngine' instead." #-}

-- | The unique identifier of the EC2 security group associated with the EMR Notebook instance. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks> in the /EMR Management Guide/ .
--
-- /Note:/ Consider using 'notebookInstanceSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neNotebookInstanceSecurityGroupId :: Lens.Lens' NotebookExecution (Lude.Maybe Lude.Text)
neNotebookInstanceSecurityGroupId = Lens.lens (notebookInstanceSecurityGroupId :: NotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceSecurityGroupId = a} :: NotebookExecution)
{-# DEPRECATED neNotebookInstanceSecurityGroupId "Use generic-lens or generic-optics with 'notebookInstanceSecurityGroupId' instead." #-}

-- | The unique identifier of the EMR Notebook that is used for the notebook execution.
--
-- /Note:/ Consider using 'editorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neEditorId :: Lens.Lens' NotebookExecution (Lude.Maybe Lude.Text)
neEditorId = Lens.lens (editorId :: NotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {editorId = a} :: NotebookExecution)
{-# DEPRECATED neEditorId "Use generic-lens or generic-optics with 'editorId' instead." #-}

-- | The timestamp when notebook execution started.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neStartTime :: Lens.Lens' NotebookExecution (Lude.Maybe Lude.Timestamp)
neStartTime = Lens.lens (startTime :: NotebookExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {startTime = a} :: NotebookExecution)
{-# DEPRECATED neStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the notebook execution.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neARN :: Lens.Lens' NotebookExecution (Lude.Maybe Lude.Text)
neARN = Lens.lens (arn :: NotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: NotebookExecution)
{-# DEPRECATED neARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The location of the notebook execution's output file in Amazon S3.
--
-- /Note:/ Consider using 'outputNotebookURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neOutputNotebookURI :: Lens.Lens' NotebookExecution (Lude.Maybe Lude.Text)
neOutputNotebookURI = Lens.lens (outputNotebookURI :: NotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {outputNotebookURI = a} :: NotebookExecution)
{-# DEPRECATED neOutputNotebookURI "Use generic-lens or generic-optics with 'outputNotebookURI' instead." #-}

-- | The unique identifier of a notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neNotebookExecutionId :: Lens.Lens' NotebookExecution (Lude.Maybe Lude.Text)
neNotebookExecutionId = Lens.lens (notebookExecutionId :: NotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {notebookExecutionId = a} :: NotebookExecution)
{-# DEPRECATED neNotebookExecutionId "Use generic-lens or generic-optics with 'notebookExecutionId' instead." #-}

-- | A name for the notebook execution.
--
-- /Note:/ Consider using 'notebookExecutionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neNotebookExecutionName :: Lens.Lens' NotebookExecution (Lude.Maybe Lude.Text)
neNotebookExecutionName = Lens.lens (notebookExecutionName :: NotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {notebookExecutionName = a} :: NotebookExecution)
{-# DEPRECATED neNotebookExecutionName "Use generic-lens or generic-optics with 'notebookExecutionName' instead." #-}

-- | The reason for the latest status change of the notebook execution.
--
-- /Note:/ Consider using 'lastStateChangeReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neLastStateChangeReason :: Lens.Lens' NotebookExecution (Lude.Maybe Lude.Text)
neLastStateChangeReason = Lens.lens (lastStateChangeReason :: NotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {lastStateChangeReason = a} :: NotebookExecution)
{-# DEPRECATED neLastStateChangeReason "Use generic-lens or generic-optics with 'lastStateChangeReason' instead." #-}

-- | The timestamp when notebook execution ended.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neEndTime :: Lens.Lens' NotebookExecution (Lude.Maybe Lude.Timestamp)
neEndTime = Lens.lens (endTime :: NotebookExecution -> Lude.Maybe Lude.Timestamp) (\s a -> s {endTime = a} :: NotebookExecution)
{-# DEPRECATED neEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | Input parameters in JSON format passed to the EMR Notebook at runtime for execution.
--
-- /Note:/ Consider using 'notebookParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neNotebookParams :: Lens.Lens' NotebookExecution (Lude.Maybe Lude.Text)
neNotebookParams = Lens.lens (notebookParams :: NotebookExecution -> Lude.Maybe Lude.Text) (\s a -> s {notebookParams = a} :: NotebookExecution)
{-# DEPRECATED neNotebookParams "Use generic-lens or generic-optics with 'notebookParams' instead." #-}

-- | A list of tags associated with a notebook execution. Tags are user-defined key-value pairs that consist of a required key string with a maximum of 128 characters and an optional value string with a maximum of 256 characters.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
neTags :: Lens.Lens' NotebookExecution (Lude.Maybe [Tag])
neTags = Lens.lens (tags :: NotebookExecution -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: NotebookExecution)
{-# DEPRECATED neTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON NotebookExecution where
  parseJSON =
    Lude.withObject
      "NotebookExecution"
      ( \x ->
          NotebookExecution'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "ExecutionEngine")
            Lude.<*> (x Lude..:? "NotebookInstanceSecurityGroupId")
            Lude.<*> (x Lude..:? "EditorId")
            Lude.<*> (x Lude..:? "StartTime")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "OutputNotebookURI")
            Lude.<*> (x Lude..:? "NotebookExecutionId")
            Lude.<*> (x Lude..:? "NotebookExecutionName")
            Lude.<*> (x Lude..:? "LastStateChangeReason")
            Lude.<*> (x Lude..:? "EndTime")
            Lude.<*> (x Lude..:? "NotebookParams")
            Lude.<*> (x Lude..:? "Tags" Lude..!= Lude.mempty)
      )
