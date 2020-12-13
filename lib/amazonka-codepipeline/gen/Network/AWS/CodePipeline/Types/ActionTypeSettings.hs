{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionTypeSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypeSettings
  ( ActionTypeSettings (..),

    -- * Smart constructor
    mkActionTypeSettings,

    -- * Lenses
    atsThirdPartyConfigurationURL,
    atsExecutionURLTemplate,
    atsRevisionURLTemplate,
    atsEntityURLTemplate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about the settings for an action type.
--
-- /See:/ 'mkActionTypeSettings' smart constructor.
data ActionTypeSettings = ActionTypeSettings'
  { -- | The URL of a sign-up page where users can sign up for an external service and perform initial configuration of the action provided by that service.
    thirdPartyConfigurationURL :: Lude.Maybe Lude.Text,
    -- | The URL returned to the AWS CodePipeline console that contains a link to the top-level landing page for the external system, such as the console page for AWS CodeDeploy. This link is shown on the pipeline view page in the AWS CodePipeline console and provides a link to the execution entity of the external action.
    executionURLTemplate :: Lude.Maybe Lude.Text,
    -- | The URL returned to the AWS CodePipeline console that contains a link to the page where customers can update or change the configuration of the external action.
    revisionURLTemplate :: Lude.Maybe Lude.Text,
    -- | The URL returned to the AWS CodePipeline console that provides a deep link to the resources of the external system, such as the configuration page for an AWS CodeDeploy deployment group. This link is provided as part of the action display in the pipeline.
    entityURLTemplate :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActionTypeSettings' with the minimum fields required to make a request.
--
-- * 'thirdPartyConfigurationURL' - The URL of a sign-up page where users can sign up for an external service and perform initial configuration of the action provided by that service.
-- * 'executionURLTemplate' - The URL returned to the AWS CodePipeline console that contains a link to the top-level landing page for the external system, such as the console page for AWS CodeDeploy. This link is shown on the pipeline view page in the AWS CodePipeline console and provides a link to the execution entity of the external action.
-- * 'revisionURLTemplate' - The URL returned to the AWS CodePipeline console that contains a link to the page where customers can update or change the configuration of the external action.
-- * 'entityURLTemplate' - The URL returned to the AWS CodePipeline console that provides a deep link to the resources of the external system, such as the configuration page for an AWS CodeDeploy deployment group. This link is provided as part of the action display in the pipeline.
mkActionTypeSettings ::
  ActionTypeSettings
mkActionTypeSettings =
  ActionTypeSettings'
    { thirdPartyConfigurationURL = Lude.Nothing,
      executionURLTemplate = Lude.Nothing,
      revisionURLTemplate = Lude.Nothing,
      entityURLTemplate = Lude.Nothing
    }

-- | The URL of a sign-up page where users can sign up for an external service and perform initial configuration of the action provided by that service.
--
-- /Note:/ Consider using 'thirdPartyConfigurationURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsThirdPartyConfigurationURL :: Lens.Lens' ActionTypeSettings (Lude.Maybe Lude.Text)
atsThirdPartyConfigurationURL = Lens.lens (thirdPartyConfigurationURL :: ActionTypeSettings -> Lude.Maybe Lude.Text) (\s a -> s {thirdPartyConfigurationURL = a} :: ActionTypeSettings)
{-# DEPRECATED atsThirdPartyConfigurationURL "Use generic-lens or generic-optics with 'thirdPartyConfigurationURL' instead." #-}

-- | The URL returned to the AWS CodePipeline console that contains a link to the top-level landing page for the external system, such as the console page for AWS CodeDeploy. This link is shown on the pipeline view page in the AWS CodePipeline console and provides a link to the execution entity of the external action.
--
-- /Note:/ Consider using 'executionURLTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsExecutionURLTemplate :: Lens.Lens' ActionTypeSettings (Lude.Maybe Lude.Text)
atsExecutionURLTemplate = Lens.lens (executionURLTemplate :: ActionTypeSettings -> Lude.Maybe Lude.Text) (\s a -> s {executionURLTemplate = a} :: ActionTypeSettings)
{-# DEPRECATED atsExecutionURLTemplate "Use generic-lens or generic-optics with 'executionURLTemplate' instead." #-}

-- | The URL returned to the AWS CodePipeline console that contains a link to the page where customers can update or change the configuration of the external action.
--
-- /Note:/ Consider using 'revisionURLTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsRevisionURLTemplate :: Lens.Lens' ActionTypeSettings (Lude.Maybe Lude.Text)
atsRevisionURLTemplate = Lens.lens (revisionURLTemplate :: ActionTypeSettings -> Lude.Maybe Lude.Text) (\s a -> s {revisionURLTemplate = a} :: ActionTypeSettings)
{-# DEPRECATED atsRevisionURLTemplate "Use generic-lens or generic-optics with 'revisionURLTemplate' instead." #-}

-- | The URL returned to the AWS CodePipeline console that provides a deep link to the resources of the external system, such as the configuration page for an AWS CodeDeploy deployment group. This link is provided as part of the action display in the pipeline.
--
-- /Note:/ Consider using 'entityURLTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsEntityURLTemplate :: Lens.Lens' ActionTypeSettings (Lude.Maybe Lude.Text)
atsEntityURLTemplate = Lens.lens (entityURLTemplate :: ActionTypeSettings -> Lude.Maybe Lude.Text) (\s a -> s {entityURLTemplate = a} :: ActionTypeSettings)
{-# DEPRECATED atsEntityURLTemplate "Use generic-lens or generic-optics with 'entityURLTemplate' instead." #-}

instance Lude.FromJSON ActionTypeSettings where
  parseJSON =
    Lude.withObject
      "ActionTypeSettings"
      ( \x ->
          ActionTypeSettings'
            Lude.<$> (x Lude..:? "thirdPartyConfigurationUrl")
            Lude.<*> (x Lude..:? "executionUrlTemplate")
            Lude.<*> (x Lude..:? "revisionUrlTemplate")
            Lude.<*> (x Lude..:? "entityUrlTemplate")
      )

instance Lude.ToJSON ActionTypeSettings where
  toJSON ActionTypeSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("thirdPartyConfigurationUrl" Lude..=)
              Lude.<$> thirdPartyConfigurationURL,
            ("executionUrlTemplate" Lude..=) Lude.<$> executionURLTemplate,
            ("revisionUrlTemplate" Lude..=) Lude.<$> revisionURLTemplate,
            ("entityUrlTemplate" Lude..=) Lude.<$> entityURLTemplate
          ]
      )
