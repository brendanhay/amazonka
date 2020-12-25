{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookDefinition
  ( WebhookDefinition (..),

    -- * Smart constructor
    mkWebhookDefinition,

    -- * Lenses
    wdName,
    wdTargetPipeline,
    wdTargetAction,
    wdFilters,
    wdAuthentication,
    wdAuthenticationConfiguration,
  )
where

import qualified Network.AWS.CodePipeline.Types.PipelineName as Types
import qualified Network.AWS.CodePipeline.Types.TargetAction as Types
import qualified Network.AWS.CodePipeline.Types.WebhookAuthConfiguration as Types
import qualified Network.AWS.CodePipeline.Types.WebhookAuthenticationType as Types
import qualified Network.AWS.CodePipeline.Types.WebhookFilterRule as Types
import qualified Network.AWS.CodePipeline.Types.WebhookName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about a webhook and its definition.
--
-- /See:/ 'mkWebhookDefinition' smart constructor.
data WebhookDefinition = WebhookDefinition'
  { -- | The name of the webhook.
    name :: Types.WebhookName,
    -- | The name of the pipeline you want to connect to the webhook.
    targetPipeline :: Types.PipelineName,
    -- | The name of the action in a pipeline you want to connect to the webhook. The action must be from the source (first) stage of the pipeline.
    targetAction :: Types.TargetAction,
    -- | A list of rules applied to the body/payload sent in the POST request to a webhook URL. All defined rules must pass for the request to be accepted and the pipeline started.
    filters :: [Types.WebhookFilterRule],
    -- | Supported options are GITHUB_HMAC, IP, and UNAUTHENTICATED.
    --
    --
    --     * For information about the authentication scheme implemented by GITHUB_HMAC, see <https://developer.github.com/webhooks/securing/ Securing your webhooks> on the GitHub Developer website.
    --
    --
    --     * IP rejects webhooks trigger requests unless they originate from an IP address in the IP range whitelisted in the authentication configuration.
    --
    --
    --     * UNAUTHENTICATED accepts all webhook trigger requests regardless of origin.
    authentication :: Types.WebhookAuthenticationType,
    -- | Properties that configure the authentication applied to incoming webhook trigger requests. The required properties depend on the authentication type. For GITHUB_HMAC, only the @SecretToken @ property must be set. For IP, only the @AllowedIPRange @ property must be set to a valid CIDR range. For UNAUTHENTICATED, no properties can be set.
    authenticationConfiguration :: Types.WebhookAuthConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WebhookDefinition' value with any optional fields omitted.
mkWebhookDefinition ::
  -- | 'name'
  Types.WebhookName ->
  -- | 'targetPipeline'
  Types.PipelineName ->
  -- | 'targetAction'
  Types.TargetAction ->
  -- | 'authentication'
  Types.WebhookAuthenticationType ->
  -- | 'authenticationConfiguration'
  Types.WebhookAuthConfiguration ->
  WebhookDefinition
mkWebhookDefinition
  name
  targetPipeline
  targetAction
  authentication
  authenticationConfiguration =
    WebhookDefinition'
      { name,
        targetPipeline,
        targetAction,
        filters = Core.mempty,
        authentication,
        authenticationConfiguration
      }

-- | The name of the webhook.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdName :: Lens.Lens' WebhookDefinition Types.WebhookName
wdName = Lens.field @"name"
{-# DEPRECATED wdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the pipeline you want to connect to the webhook.
--
-- /Note:/ Consider using 'targetPipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdTargetPipeline :: Lens.Lens' WebhookDefinition Types.PipelineName
wdTargetPipeline = Lens.field @"targetPipeline"
{-# DEPRECATED wdTargetPipeline "Use generic-lens or generic-optics with 'targetPipeline' instead." #-}

-- | The name of the action in a pipeline you want to connect to the webhook. The action must be from the source (first) stage of the pipeline.
--
-- /Note:/ Consider using 'targetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdTargetAction :: Lens.Lens' WebhookDefinition Types.TargetAction
wdTargetAction = Lens.field @"targetAction"
{-# DEPRECATED wdTargetAction "Use generic-lens or generic-optics with 'targetAction' instead." #-}

-- | A list of rules applied to the body/payload sent in the POST request to a webhook URL. All defined rules must pass for the request to be accepted and the pipeline started.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdFilters :: Lens.Lens' WebhookDefinition [Types.WebhookFilterRule]
wdFilters = Lens.field @"filters"
{-# DEPRECATED wdFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | Supported options are GITHUB_HMAC, IP, and UNAUTHENTICATED.
--
--
--     * For information about the authentication scheme implemented by GITHUB_HMAC, see <https://developer.github.com/webhooks/securing/ Securing your webhooks> on the GitHub Developer website.
--
--
--     * IP rejects webhooks trigger requests unless they originate from an IP address in the IP range whitelisted in the authentication configuration.
--
--
--     * UNAUTHENTICATED accepts all webhook trigger requests regardless of origin.
--
--
--
-- /Note:/ Consider using 'authentication' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdAuthentication :: Lens.Lens' WebhookDefinition Types.WebhookAuthenticationType
wdAuthentication = Lens.field @"authentication"
{-# DEPRECATED wdAuthentication "Use generic-lens or generic-optics with 'authentication' instead." #-}

-- | Properties that configure the authentication applied to incoming webhook trigger requests. The required properties depend on the authentication type. For GITHUB_HMAC, only the @SecretToken @ property must be set. For IP, only the @AllowedIPRange @ property must be set to a valid CIDR range. For UNAUTHENTICATED, no properties can be set.
--
-- /Note:/ Consider using 'authenticationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdAuthenticationConfiguration :: Lens.Lens' WebhookDefinition Types.WebhookAuthConfiguration
wdAuthenticationConfiguration = Lens.field @"authenticationConfiguration"
{-# DEPRECATED wdAuthenticationConfiguration "Use generic-lens or generic-optics with 'authenticationConfiguration' instead." #-}

instance Core.FromJSON WebhookDefinition where
  toJSON WebhookDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("targetPipeline" Core..= targetPipeline),
            Core.Just ("targetAction" Core..= targetAction),
            Core.Just ("filters" Core..= filters),
            Core.Just ("authentication" Core..= authentication),
            Core.Just
              ( "authenticationConfiguration"
                  Core..= authenticationConfiguration
              )
          ]
      )

instance Core.FromJSON WebhookDefinition where
  parseJSON =
    Core.withObject "WebhookDefinition" Core.$
      \x ->
        WebhookDefinition'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..: "targetPipeline")
          Core.<*> (x Core..: "targetAction")
          Core.<*> (x Core..:? "filters" Core..!= Core.mempty)
          Core.<*> (x Core..: "authentication")
          Core.<*> (x Core..: "authenticationConfiguration")
