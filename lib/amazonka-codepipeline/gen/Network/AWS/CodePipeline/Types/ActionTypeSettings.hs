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
    atsEntityUrlTemplate,
    atsExecutionUrlTemplate,
    atsRevisionUrlTemplate,
    atsThirdPartyConfigurationUrl,
  )
where

import qualified Network.AWS.CodePipeline.Types.EntityUrlTemplate as Types
import qualified Network.AWS.CodePipeline.Types.ExecutionUrlTemplate as Types
import qualified Network.AWS.CodePipeline.Types.RevisionUrlTemplate as Types
import qualified Network.AWS.CodePipeline.Types.ThirdPartyConfigurationUrl as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about the settings for an action type.
--
-- /See:/ 'mkActionTypeSettings' smart constructor.
data ActionTypeSettings = ActionTypeSettings'
  { -- | The URL returned to the AWS CodePipeline console that provides a deep link to the resources of the external system, such as the configuration page for an AWS CodeDeploy deployment group. This link is provided as part of the action display in the pipeline.
    entityUrlTemplate :: Core.Maybe Types.EntityUrlTemplate,
    -- | The URL returned to the AWS CodePipeline console that contains a link to the top-level landing page for the external system, such as the console page for AWS CodeDeploy. This link is shown on the pipeline view page in the AWS CodePipeline console and provides a link to the execution entity of the external action.
    executionUrlTemplate :: Core.Maybe Types.ExecutionUrlTemplate,
    -- | The URL returned to the AWS CodePipeline console that contains a link to the page where customers can update or change the configuration of the external action.
    revisionUrlTemplate :: Core.Maybe Types.RevisionUrlTemplate,
    -- | The URL of a sign-up page where users can sign up for an external service and perform initial configuration of the action provided by that service.
    thirdPartyConfigurationUrl :: Core.Maybe Types.ThirdPartyConfigurationUrl
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ActionTypeSettings' value with any optional fields omitted.
mkActionTypeSettings ::
  ActionTypeSettings
mkActionTypeSettings =
  ActionTypeSettings'
    { entityUrlTemplate = Core.Nothing,
      executionUrlTemplate = Core.Nothing,
      revisionUrlTemplate = Core.Nothing,
      thirdPartyConfigurationUrl = Core.Nothing
    }

-- | The URL returned to the AWS CodePipeline console that provides a deep link to the resources of the external system, such as the configuration page for an AWS CodeDeploy deployment group. This link is provided as part of the action display in the pipeline.
--
-- /Note:/ Consider using 'entityUrlTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsEntityUrlTemplate :: Lens.Lens' ActionTypeSettings (Core.Maybe Types.EntityUrlTemplate)
atsEntityUrlTemplate = Lens.field @"entityUrlTemplate"
{-# DEPRECATED atsEntityUrlTemplate "Use generic-lens or generic-optics with 'entityUrlTemplate' instead." #-}

-- | The URL returned to the AWS CodePipeline console that contains a link to the top-level landing page for the external system, such as the console page for AWS CodeDeploy. This link is shown on the pipeline view page in the AWS CodePipeline console and provides a link to the execution entity of the external action.
--
-- /Note:/ Consider using 'executionUrlTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsExecutionUrlTemplate :: Lens.Lens' ActionTypeSettings (Core.Maybe Types.ExecutionUrlTemplate)
atsExecutionUrlTemplate = Lens.field @"executionUrlTemplate"
{-# DEPRECATED atsExecutionUrlTemplate "Use generic-lens or generic-optics with 'executionUrlTemplate' instead." #-}

-- | The URL returned to the AWS CodePipeline console that contains a link to the page where customers can update or change the configuration of the external action.
--
-- /Note:/ Consider using 'revisionUrlTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsRevisionUrlTemplate :: Lens.Lens' ActionTypeSettings (Core.Maybe Types.RevisionUrlTemplate)
atsRevisionUrlTemplate = Lens.field @"revisionUrlTemplate"
{-# DEPRECATED atsRevisionUrlTemplate "Use generic-lens or generic-optics with 'revisionUrlTemplate' instead." #-}

-- | The URL of a sign-up page where users can sign up for an external service and perform initial configuration of the action provided by that service.
--
-- /Note:/ Consider using 'thirdPartyConfigurationUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atsThirdPartyConfigurationUrl :: Lens.Lens' ActionTypeSettings (Core.Maybe Types.ThirdPartyConfigurationUrl)
atsThirdPartyConfigurationUrl = Lens.field @"thirdPartyConfigurationUrl"
{-# DEPRECATED atsThirdPartyConfigurationUrl "Use generic-lens or generic-optics with 'thirdPartyConfigurationUrl' instead." #-}

instance Core.FromJSON ActionTypeSettings where
  toJSON ActionTypeSettings {..} =
    Core.object
      ( Core.catMaybes
          [ ("entityUrlTemplate" Core..=) Core.<$> entityUrlTemplate,
            ("executionUrlTemplate" Core..=) Core.<$> executionUrlTemplate,
            ("revisionUrlTemplate" Core..=) Core.<$> revisionUrlTemplate,
            ("thirdPartyConfigurationUrl" Core..=)
              Core.<$> thirdPartyConfigurationUrl
          ]
      )

instance Core.FromJSON ActionTypeSettings where
  parseJSON =
    Core.withObject "ActionTypeSettings" Core.$
      \x ->
        ActionTypeSettings'
          Core.<$> (x Core..:? "entityUrlTemplate")
          Core.<*> (x Core..:? "executionUrlTemplate")
          Core.<*> (x Core..:? "revisionUrlTemplate")
          Core.<*> (x Core..:? "thirdPartyConfigurationUrl")
