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
    wdTargetAction,
    wdAuthentication,
    wdFilters,
    wdTargetPipeline,
    wdName,
    wdAuthenticationConfiguration,
  )
where

import Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
import Network.AWS.CodePipeline.Types.WebhookAuthenticationType
import Network.AWS.CodePipeline.Types.WebhookFilterRule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about a webhook and its definition.
--
-- /See:/ 'mkWebhookDefinition' smart constructor.
data WebhookDefinition = WebhookDefinition'
  { -- | The name of the action in a pipeline you want to connect to the webhook. The action must be from the source (first) stage of the pipeline.
    targetAction :: Lude.Text,
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
    authentication :: WebhookAuthenticationType,
    -- | A list of rules applied to the body/payload sent in the POST request to a webhook URL. All defined rules must pass for the request to be accepted and the pipeline started.
    filters :: [WebhookFilterRule],
    -- | The name of the pipeline you want to connect to the webhook.
    targetPipeline :: Lude.Text,
    -- | The name of the webhook.
    name :: Lude.Text,
    -- | Properties that configure the authentication applied to incoming webhook trigger requests. The required properties depend on the authentication type. For GITHUB_HMAC, only the @SecretToken @ property must be set. For IP, only the @AllowedIPRange @ property must be set to a valid CIDR range. For UNAUTHENTICATED, no properties can be set.
    authenticationConfiguration :: WebhookAuthConfiguration
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WebhookDefinition' with the minimum fields required to make a request.
--
-- * 'targetAction' - The name of the action in a pipeline you want to connect to the webhook. The action must be from the source (first) stage of the pipeline.
-- * 'authentication' - Supported options are GITHUB_HMAC, IP, and UNAUTHENTICATED.
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
-- * 'filters' - A list of rules applied to the body/payload sent in the POST request to a webhook URL. All defined rules must pass for the request to be accepted and the pipeline started.
-- * 'targetPipeline' - The name of the pipeline you want to connect to the webhook.
-- * 'name' - The name of the webhook.
-- * 'authenticationConfiguration' - Properties that configure the authentication applied to incoming webhook trigger requests. The required properties depend on the authentication type. For GITHUB_HMAC, only the @SecretToken @ property must be set. For IP, only the @AllowedIPRange @ property must be set to a valid CIDR range. For UNAUTHENTICATED, no properties can be set.
mkWebhookDefinition ::
  -- | 'targetAction'
  Lude.Text ->
  -- | 'authentication'
  WebhookAuthenticationType ->
  -- | 'targetPipeline'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'authenticationConfiguration'
  WebhookAuthConfiguration ->
  WebhookDefinition
mkWebhookDefinition
  pTargetAction_
  pAuthentication_
  pTargetPipeline_
  pName_
  pAuthenticationConfiguration_ =
    WebhookDefinition'
      { targetAction = pTargetAction_,
        authentication = pAuthentication_,
        filters = Lude.mempty,
        targetPipeline = pTargetPipeline_,
        name = pName_,
        authenticationConfiguration = pAuthenticationConfiguration_
      }

-- | The name of the action in a pipeline you want to connect to the webhook. The action must be from the source (first) stage of the pipeline.
--
-- /Note:/ Consider using 'targetAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdTargetAction :: Lens.Lens' WebhookDefinition Lude.Text
wdTargetAction = Lens.lens (targetAction :: WebhookDefinition -> Lude.Text) (\s a -> s {targetAction = a} :: WebhookDefinition)
{-# DEPRECATED wdTargetAction "Use generic-lens or generic-optics with 'targetAction' instead." #-}

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
wdAuthentication :: Lens.Lens' WebhookDefinition WebhookAuthenticationType
wdAuthentication = Lens.lens (authentication :: WebhookDefinition -> WebhookAuthenticationType) (\s a -> s {authentication = a} :: WebhookDefinition)
{-# DEPRECATED wdAuthentication "Use generic-lens or generic-optics with 'authentication' instead." #-}

-- | A list of rules applied to the body/payload sent in the POST request to a webhook URL. All defined rules must pass for the request to be accepted and the pipeline started.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdFilters :: Lens.Lens' WebhookDefinition [WebhookFilterRule]
wdFilters = Lens.lens (filters :: WebhookDefinition -> [WebhookFilterRule]) (\s a -> s {filters = a} :: WebhookDefinition)
{-# DEPRECATED wdFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The name of the pipeline you want to connect to the webhook.
--
-- /Note:/ Consider using 'targetPipeline' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdTargetPipeline :: Lens.Lens' WebhookDefinition Lude.Text
wdTargetPipeline = Lens.lens (targetPipeline :: WebhookDefinition -> Lude.Text) (\s a -> s {targetPipeline = a} :: WebhookDefinition)
{-# DEPRECATED wdTargetPipeline "Use generic-lens or generic-optics with 'targetPipeline' instead." #-}

-- | The name of the webhook.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdName :: Lens.Lens' WebhookDefinition Lude.Text
wdName = Lens.lens (name :: WebhookDefinition -> Lude.Text) (\s a -> s {name = a} :: WebhookDefinition)
{-# DEPRECATED wdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Properties that configure the authentication applied to incoming webhook trigger requests. The required properties depend on the authentication type. For GITHUB_HMAC, only the @SecretToken @ property must be set. For IP, only the @AllowedIPRange @ property must be set to a valid CIDR range. For UNAUTHENTICATED, no properties can be set.
--
-- /Note:/ Consider using 'authenticationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wdAuthenticationConfiguration :: Lens.Lens' WebhookDefinition WebhookAuthConfiguration
wdAuthenticationConfiguration = Lens.lens (authenticationConfiguration :: WebhookDefinition -> WebhookAuthConfiguration) (\s a -> s {authenticationConfiguration = a} :: WebhookDefinition)
{-# DEPRECATED wdAuthenticationConfiguration "Use generic-lens or generic-optics with 'authenticationConfiguration' instead." #-}

instance Lude.FromJSON WebhookDefinition where
  parseJSON =
    Lude.withObject
      "WebhookDefinition"
      ( \x ->
          WebhookDefinition'
            Lude.<$> (x Lude..: "targetAction")
            Lude.<*> (x Lude..: "authentication")
            Lude.<*> (x Lude..:? "filters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "targetPipeline")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "authenticationConfiguration")
      )

instance Lude.ToJSON WebhookDefinition where
  toJSON WebhookDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("targetAction" Lude..= targetAction),
            Lude.Just ("authentication" Lude..= authentication),
            Lude.Just ("filters" Lude..= filters),
            Lude.Just ("targetPipeline" Lude..= targetPipeline),
            Lude.Just ("name" Lude..= name),
            Lude.Just
              ( "authenticationConfiguration"
                  Lude..= authenticationConfiguration
              )
          ]
      )
