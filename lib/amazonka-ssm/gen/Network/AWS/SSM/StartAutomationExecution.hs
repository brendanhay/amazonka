{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.StartAutomationExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates execution of an Automation document.
module Network.AWS.SSM.StartAutomationExecution
  ( -- * Creating a request
    StartAutomationExecution (..),
    mkStartAutomationExecution,

    -- ** Request lenses
    saeTargetParameterName,
    saeTargetLocations,
    saeClientToken,
    saeDocumentName,
    saeMode,
    saeTargetMaps,
    saeMaxErrors,
    saeTargets,
    saeParameters,
    saeDocumentVersion,
    saeTags,
    saeMaxConcurrency,

    -- * Destructuring the response
    StartAutomationExecutionResponse (..),
    mkStartAutomationExecutionResponse,

    -- ** Response lenses
    saersAutomationExecutionId,
    saersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkStartAutomationExecution' smart constructor.
data StartAutomationExecution = StartAutomationExecution'
  { -- | The name of the parameter used as the target resource for the rate-controlled execution. Required if you specify targets.
    targetParameterName :: Lude.Maybe Lude.Text,
    -- | A location is a combination of AWS Regions and/or AWS accounts where you want to run the Automation. Use this action to start an Automation in multiple Regions and multiple accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
    targetLocations :: Lude.Maybe (Lude.NonEmpty TargetLocation),
    -- | User-provided idempotency token. The token must be unique, is case insensitive, enforces the UUID format, and can't be reused.
    clientToken :: Lude.Maybe Lude.Text,
    -- | The name of the Automation document to use for this execution.
    documentName :: Lude.Text,
    -- | The execution mode of the automation. Valid modes include the following: Auto and Interactive. The default mode is Auto.
    mode :: Lude.Maybe ExecutionMode,
    -- | A key-value mapping of document parameters to target resources. Both Targets and TargetMaps cannot be specified together.
    targetMaps :: Lude.Maybe [Lude.HashMap Lude.Text ([Lude.Text])],
    -- | The number of errors that are allowed before the system stops running the automation on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops running the automation when the fourth error is received. If you specify 0, then the system stops running the automation on additional targets after the first error result is returned. If you run an automation on 50 resources and set max-errors to 10%, then the system stops running the automation on additional targets when the sixth error is received.
    --
    -- Executions that are already running an automation when max-errors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set max-concurrency to 1 so the executions proceed one at a time.
    maxErrors :: Lude.Maybe Lude.Text,
    -- | A key-value mapping to target resources. Required if you specify TargetParameterName.
    targets :: Lude.Maybe [Target],
    -- | A key-value map of execution parameters, which match the declared parameters in the Automation document.
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    -- | The version of the Automation document to use for this execution.
    documentVersion :: Lude.Maybe Lude.Text,
    -- | Optional metadata that you assign to a resource. You can specify a maximum of five tags for an automation. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an automation to identify an environment or operating system. In this case, you could specify the following key name/value pairs:
    --
    --
    --     * @Key=environment,Value=test@
    --
    --
    --     * @Key=OS,Value=Windows@
    tags :: Lude.Maybe [Tag],
    -- | The maximum number of targets allowed to run this task in parallel. You can specify a number, such as 10, or a percentage, such as 10%. The default value is 10.
    maxConcurrency :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAutomationExecution' with the minimum fields required to make a request.
--
-- * 'targetParameterName' - The name of the parameter used as the target resource for the rate-controlled execution. Required if you specify targets.
-- * 'targetLocations' - A location is a combination of AWS Regions and/or AWS accounts where you want to run the Automation. Use this action to start an Automation in multiple Regions and multiple accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
-- * 'clientToken' - User-provided idempotency token. The token must be unique, is case insensitive, enforces the UUID format, and can't be reused.
-- * 'documentName' - The name of the Automation document to use for this execution.
-- * 'mode' - The execution mode of the automation. Valid modes include the following: Auto and Interactive. The default mode is Auto.
-- * 'targetMaps' - A key-value mapping of document parameters to target resources. Both Targets and TargetMaps cannot be specified together.
-- * 'maxErrors' - The number of errors that are allowed before the system stops running the automation on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops running the automation when the fourth error is received. If you specify 0, then the system stops running the automation on additional targets after the first error result is returned. If you run an automation on 50 resources and set max-errors to 10%, then the system stops running the automation on additional targets when the sixth error is received.
--
-- Executions that are already running an automation when max-errors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set max-concurrency to 1 so the executions proceed one at a time.
-- * 'targets' - A key-value mapping to target resources. Required if you specify TargetParameterName.
-- * 'parameters' - A key-value map of execution parameters, which match the declared parameters in the Automation document.
-- * 'documentVersion' - The version of the Automation document to use for this execution.
-- * 'tags' - Optional metadata that you assign to a resource. You can specify a maximum of five tags for an automation. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an automation to identify an environment or operating system. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=environment,Value=test@
--
--
--     * @Key=OS,Value=Windows@
--
--
-- * 'maxConcurrency' - The maximum number of targets allowed to run this task in parallel. You can specify a number, such as 10, or a percentage, such as 10%. The default value is 10.
mkStartAutomationExecution ::
  -- | 'documentName'
  Lude.Text ->
  StartAutomationExecution
mkStartAutomationExecution pDocumentName_ =
  StartAutomationExecution'
    { targetParameterName = Lude.Nothing,
      targetLocations = Lude.Nothing,
      clientToken = Lude.Nothing,
      documentName = pDocumentName_,
      mode = Lude.Nothing,
      targetMaps = Lude.Nothing,
      maxErrors = Lude.Nothing,
      targets = Lude.Nothing,
      parameters = Lude.Nothing,
      documentVersion = Lude.Nothing,
      tags = Lude.Nothing,
      maxConcurrency = Lude.Nothing
    }

-- | The name of the parameter used as the target resource for the rate-controlled execution. Required if you specify targets.
--
-- /Note:/ Consider using 'targetParameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeTargetParameterName :: Lens.Lens' StartAutomationExecution (Lude.Maybe Lude.Text)
saeTargetParameterName = Lens.lens (targetParameterName :: StartAutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {targetParameterName = a} :: StartAutomationExecution)
{-# DEPRECATED saeTargetParameterName "Use generic-lens or generic-optics with 'targetParameterName' instead." #-}

-- | A location is a combination of AWS Regions and/or AWS accounts where you want to run the Automation. Use this action to start an Automation in multiple Regions and multiple accounts. For more information, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/systems-manager-automation-multiple-accounts-and-regions.html Running Automation workflows in multiple AWS Regions and accounts> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'targetLocations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeTargetLocations :: Lens.Lens' StartAutomationExecution (Lude.Maybe (Lude.NonEmpty TargetLocation))
saeTargetLocations = Lens.lens (targetLocations :: StartAutomationExecution -> Lude.Maybe (Lude.NonEmpty TargetLocation)) (\s a -> s {targetLocations = a} :: StartAutomationExecution)
{-# DEPRECATED saeTargetLocations "Use generic-lens or generic-optics with 'targetLocations' instead." #-}

-- | User-provided idempotency token. The token must be unique, is case insensitive, enforces the UUID format, and can't be reused.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeClientToken :: Lens.Lens' StartAutomationExecution (Lude.Maybe Lude.Text)
saeClientToken = Lens.lens (clientToken :: StartAutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: StartAutomationExecution)
{-# DEPRECATED saeClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The name of the Automation document to use for this execution.
--
-- /Note:/ Consider using 'documentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeDocumentName :: Lens.Lens' StartAutomationExecution Lude.Text
saeDocumentName = Lens.lens (documentName :: StartAutomationExecution -> Lude.Text) (\s a -> s {documentName = a} :: StartAutomationExecution)
{-# DEPRECATED saeDocumentName "Use generic-lens or generic-optics with 'documentName' instead." #-}

-- | The execution mode of the automation. Valid modes include the following: Auto and Interactive. The default mode is Auto.
--
-- /Note:/ Consider using 'mode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeMode :: Lens.Lens' StartAutomationExecution (Lude.Maybe ExecutionMode)
saeMode = Lens.lens (mode :: StartAutomationExecution -> Lude.Maybe ExecutionMode) (\s a -> s {mode = a} :: StartAutomationExecution)
{-# DEPRECATED saeMode "Use generic-lens or generic-optics with 'mode' instead." #-}

-- | A key-value mapping of document parameters to target resources. Both Targets and TargetMaps cannot be specified together.
--
-- /Note:/ Consider using 'targetMaps' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeTargetMaps :: Lens.Lens' StartAutomationExecution (Lude.Maybe [Lude.HashMap Lude.Text ([Lude.Text])])
saeTargetMaps = Lens.lens (targetMaps :: StartAutomationExecution -> Lude.Maybe [Lude.HashMap Lude.Text ([Lude.Text])]) (\s a -> s {targetMaps = a} :: StartAutomationExecution)
{-# DEPRECATED saeTargetMaps "Use generic-lens or generic-optics with 'targetMaps' instead." #-}

-- | The number of errors that are allowed before the system stops running the automation on additional targets. You can specify either an absolute number of errors, for example 10, or a percentage of the target set, for example 10%. If you specify 3, for example, the system stops running the automation when the fourth error is received. If you specify 0, then the system stops running the automation on additional targets after the first error result is returned. If you run an automation on 50 resources and set max-errors to 10%, then the system stops running the automation on additional targets when the sixth error is received.
--
-- Executions that are already running an automation when max-errors is reached are allowed to complete, but some of these executions may fail as well. If you need to ensure that there won't be more than max-errors failed executions, set max-concurrency to 1 so the executions proceed one at a time.
--
-- /Note:/ Consider using 'maxErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeMaxErrors :: Lens.Lens' StartAutomationExecution (Lude.Maybe Lude.Text)
saeMaxErrors = Lens.lens (maxErrors :: StartAutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {maxErrors = a} :: StartAutomationExecution)
{-# DEPRECATED saeMaxErrors "Use generic-lens or generic-optics with 'maxErrors' instead." #-}

-- | A key-value mapping to target resources. Required if you specify TargetParameterName.
--
-- /Note:/ Consider using 'targets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeTargets :: Lens.Lens' StartAutomationExecution (Lude.Maybe [Target])
saeTargets = Lens.lens (targets :: StartAutomationExecution -> Lude.Maybe [Target]) (\s a -> s {targets = a} :: StartAutomationExecution)
{-# DEPRECATED saeTargets "Use generic-lens or generic-optics with 'targets' instead." #-}

-- | A key-value map of execution parameters, which match the declared parameters in the Automation document.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeParameters :: Lens.Lens' StartAutomationExecution (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
saeParameters = Lens.lens (parameters :: StartAutomationExecution -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {parameters = a} :: StartAutomationExecution)
{-# DEPRECATED saeParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The version of the Automation document to use for this execution.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeDocumentVersion :: Lens.Lens' StartAutomationExecution (Lude.Maybe Lude.Text)
saeDocumentVersion = Lens.lens (documentVersion :: StartAutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {documentVersion = a} :: StartAutomationExecution)
{-# DEPRECATED saeDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | Optional metadata that you assign to a resource. You can specify a maximum of five tags for an automation. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag an automation to identify an environment or operating system. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=environment,Value=test@
--
--
--     * @Key=OS,Value=Windows@
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeTags :: Lens.Lens' StartAutomationExecution (Lude.Maybe [Tag])
saeTags = Lens.lens (tags :: StartAutomationExecution -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: StartAutomationExecution)
{-# DEPRECATED saeTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The maximum number of targets allowed to run this task in parallel. You can specify a number, such as 10, or a percentage, such as 10%. The default value is 10.
--
-- /Note:/ Consider using 'maxConcurrency' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saeMaxConcurrency :: Lens.Lens' StartAutomationExecution (Lude.Maybe Lude.Text)
saeMaxConcurrency = Lens.lens (maxConcurrency :: StartAutomationExecution -> Lude.Maybe Lude.Text) (\s a -> s {maxConcurrency = a} :: StartAutomationExecution)
{-# DEPRECATED saeMaxConcurrency "Use generic-lens or generic-optics with 'maxConcurrency' instead." #-}

instance Lude.AWSRequest StartAutomationExecution where
  type Rs StartAutomationExecution = StartAutomationExecutionResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartAutomationExecutionResponse'
            Lude.<$> (x Lude..?> "AutomationExecutionId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartAutomationExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.StartAutomationExecution" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartAutomationExecution where
  toJSON StartAutomationExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TargetParameterName" Lude..=) Lude.<$> targetParameterName,
            ("TargetLocations" Lude..=) Lude.<$> targetLocations,
            ("ClientToken" Lude..=) Lude.<$> clientToken,
            Lude.Just ("DocumentName" Lude..= documentName),
            ("Mode" Lude..=) Lude.<$> mode,
            ("TargetMaps" Lude..=) Lude.<$> targetMaps,
            ("MaxErrors" Lude..=) Lude.<$> maxErrors,
            ("Targets" Lude..=) Lude.<$> targets,
            ("Parameters" Lude..=) Lude.<$> parameters,
            ("DocumentVersion" Lude..=) Lude.<$> documentVersion,
            ("Tags" Lude..=) Lude.<$> tags,
            ("MaxConcurrency" Lude..=) Lude.<$> maxConcurrency
          ]
      )

instance Lude.ToPath StartAutomationExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery StartAutomationExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartAutomationExecutionResponse' smart constructor.
data StartAutomationExecutionResponse = StartAutomationExecutionResponse'
  { -- | The unique ID of a newly scheduled automation execution.
    automationExecutionId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartAutomationExecutionResponse' with the minimum fields required to make a request.
--
-- * 'automationExecutionId' - The unique ID of a newly scheduled automation execution.
-- * 'responseStatus' - The response status code.
mkStartAutomationExecutionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartAutomationExecutionResponse
mkStartAutomationExecutionResponse pResponseStatus_ =
  StartAutomationExecutionResponse'
    { automationExecutionId =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique ID of a newly scheduled automation execution.
--
-- /Note:/ Consider using 'automationExecutionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saersAutomationExecutionId :: Lens.Lens' StartAutomationExecutionResponse (Lude.Maybe Lude.Text)
saersAutomationExecutionId = Lens.lens (automationExecutionId :: StartAutomationExecutionResponse -> Lude.Maybe Lude.Text) (\s a -> s {automationExecutionId = a} :: StartAutomationExecutionResponse)
{-# DEPRECATED saersAutomationExecutionId "Use generic-lens or generic-optics with 'automationExecutionId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saersResponseStatus :: Lens.Lens' StartAutomationExecutionResponse Lude.Int
saersResponseStatus = Lens.lens (responseStatus :: StartAutomationExecutionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartAutomationExecutionResponse)
{-# DEPRECATED saersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
