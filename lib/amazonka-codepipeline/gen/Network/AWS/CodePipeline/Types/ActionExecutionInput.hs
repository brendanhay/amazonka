-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionExecutionInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionExecutionInput
  ( ActionExecutionInput (..),

    -- * Smart constructor
    mkActionExecutionInput,

    -- * Lenses
    aeiNamespace,
    aeiResolvedConfiguration,
    aeiRegion,
    aeiConfiguration,
    aeiActionTypeId,
    aeiInputArtifacts,
    aeiRoleARN,
  )
where

import Network.AWS.CodePipeline.Types.ActionTypeId
import Network.AWS.CodePipeline.Types.ArtifactDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Input information used for an action execution.
--
-- /See:/ 'mkActionExecutionInput' smart constructor.
data ActionExecutionInput = ActionExecutionInput'
  { namespace ::
      Lude.Maybe Lude.Text,
    resolvedConfiguration ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    region :: Lude.Maybe Lude.Text,
    configuration ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    actionTypeId :: Lude.Maybe ActionTypeId,
    inputArtifacts :: Lude.Maybe [ArtifactDetail],
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionExecutionInput' with the minimum fields required to make a request.
--
-- * 'actionTypeId' - Undocumented field.
-- * 'configuration' - Configuration data for an action execution.
-- * 'inputArtifacts' - Details of input artifacts of the action that correspond to the action execution.
-- * 'namespace' - The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
-- * 'region' - The AWS Region for the action, such as us-east-1.
-- * 'resolvedConfiguration' - Configuration data for an action execution with all variable references replaced with their real values for the execution.
-- * 'roleARN' - The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
mkActionExecutionInput ::
  ActionExecutionInput
mkActionExecutionInput =
  ActionExecutionInput'
    { namespace = Lude.Nothing,
      resolvedConfiguration = Lude.Nothing,
      region = Lude.Nothing,
      configuration = Lude.Nothing,
      actionTypeId = Lude.Nothing,
      inputArtifacts = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The variable namespace associated with the action. All variables produced as output by this action fall under this namespace.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiNamespace :: Lens.Lens' ActionExecutionInput (Lude.Maybe Lude.Text)
aeiNamespace = Lens.lens (namespace :: ActionExecutionInput -> Lude.Maybe Lude.Text) (\s a -> s {namespace = a} :: ActionExecutionInput)
{-# DEPRECATED aeiNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | Configuration data for an action execution with all variable references replaced with their real values for the execution.
--
-- /Note:/ Consider using 'resolvedConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiResolvedConfiguration :: Lens.Lens' ActionExecutionInput (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
aeiResolvedConfiguration = Lens.lens (resolvedConfiguration :: ActionExecutionInput -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {resolvedConfiguration = a} :: ActionExecutionInput)
{-# DEPRECATED aeiResolvedConfiguration "Use generic-lens or generic-optics with 'resolvedConfiguration' instead." #-}

-- | The AWS Region for the action, such as us-east-1.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiRegion :: Lens.Lens' ActionExecutionInput (Lude.Maybe Lude.Text)
aeiRegion = Lens.lens (region :: ActionExecutionInput -> Lude.Maybe Lude.Text) (\s a -> s {region = a} :: ActionExecutionInput)
{-# DEPRECATED aeiRegion "Use generic-lens or generic-optics with 'region' instead." #-}

-- | Configuration data for an action execution.
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiConfiguration :: Lens.Lens' ActionExecutionInput (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
aeiConfiguration = Lens.lens (configuration :: ActionExecutionInput -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {configuration = a} :: ActionExecutionInput)
{-# DEPRECATED aeiConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'actionTypeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiActionTypeId :: Lens.Lens' ActionExecutionInput (Lude.Maybe ActionTypeId)
aeiActionTypeId = Lens.lens (actionTypeId :: ActionExecutionInput -> Lude.Maybe ActionTypeId) (\s a -> s {actionTypeId = a} :: ActionExecutionInput)
{-# DEPRECATED aeiActionTypeId "Use generic-lens or generic-optics with 'actionTypeId' instead." #-}

-- | Details of input artifacts of the action that correspond to the action execution.
--
-- /Note:/ Consider using 'inputArtifacts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiInputArtifacts :: Lens.Lens' ActionExecutionInput (Lude.Maybe [ArtifactDetail])
aeiInputArtifacts = Lens.lens (inputArtifacts :: ActionExecutionInput -> Lude.Maybe [ArtifactDetail]) (\s a -> s {inputArtifacts = a} :: ActionExecutionInput)
{-# DEPRECATED aeiInputArtifacts "Use generic-lens or generic-optics with 'inputArtifacts' instead." #-}

-- | The ARN of the IAM service role that performs the declared action. This is assumed through the roleArn for the pipeline.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeiRoleARN :: Lens.Lens' ActionExecutionInput (Lude.Maybe Lude.Text)
aeiRoleARN = Lens.lens (roleARN :: ActionExecutionInput -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: ActionExecutionInput)
{-# DEPRECATED aeiRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON ActionExecutionInput where
  parseJSON =
    Lude.withObject
      "ActionExecutionInput"
      ( \x ->
          ActionExecutionInput'
            Lude.<$> (x Lude..:? "namespace")
            Lude.<*> (x Lude..:? "resolvedConfiguration" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "region")
            Lude.<*> (x Lude..:? "configuration" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "actionTypeId")
            Lude.<*> (x Lude..:? "inputArtifacts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "roleArn")
      )
