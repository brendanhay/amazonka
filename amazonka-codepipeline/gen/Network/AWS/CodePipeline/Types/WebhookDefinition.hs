{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookDefinition where

import Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
import Network.AWS.CodePipeline.Types.WebhookAuthenticationType
import Network.AWS.CodePipeline.Types.WebhookFilterRule
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about a webhook and its definition.
--
-- /See:/ 'newWebhookDefinition' smart constructor.
data WebhookDefinition = WebhookDefinition'
  { -- | The name of the webhook.
    name :: Prelude.Text,
    -- | The name of the pipeline you want to connect to the webhook.
    targetPipeline :: Prelude.Text,
    -- | The name of the action in a pipeline you want to connect to the webhook.
    -- The action must be from the source (first) stage of the pipeline.
    targetAction :: Prelude.Text,
    -- | A list of rules applied to the body\/payload sent in the POST request to
    -- a webhook URL. All defined rules must pass for the request to be
    -- accepted and the pipeline started.
    filters :: [WebhookFilterRule],
    -- | Supported options are GITHUB_HMAC, IP, and UNAUTHENTICATED.
    --
    -- -   For information about the authentication scheme implemented by
    --     GITHUB_HMAC, see
    --     <https://developer.github.com/webhooks/securing/ Securing your webhooks>
    --     on the GitHub Developer website.
    --
    -- -   IP rejects webhooks trigger requests unless they originate from an
    --     IP address in the IP range whitelisted in the authentication
    --     configuration.
    --
    -- -   UNAUTHENTICATED accepts all webhook trigger requests regardless of
    --     origin.
    authentication :: WebhookAuthenticationType,
    -- | Properties that configure the authentication applied to incoming webhook
    -- trigger requests. The required properties depend on the authentication
    -- type. For GITHUB_HMAC, only the @SecretToken @property must be set. For
    -- IP, only the @AllowedIPRange @property must be set to a valid CIDR
    -- range. For UNAUTHENTICATED, no properties can be set.
    authenticationConfiguration :: WebhookAuthConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'WebhookDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'webhookDefinition_name' - The name of the webhook.
--
-- 'targetPipeline', 'webhookDefinition_targetPipeline' - The name of the pipeline you want to connect to the webhook.
--
-- 'targetAction', 'webhookDefinition_targetAction' - The name of the action in a pipeline you want to connect to the webhook.
-- The action must be from the source (first) stage of the pipeline.
--
-- 'filters', 'webhookDefinition_filters' - A list of rules applied to the body\/payload sent in the POST request to
-- a webhook URL. All defined rules must pass for the request to be
-- accepted and the pipeline started.
--
-- 'authentication', 'webhookDefinition_authentication' - Supported options are GITHUB_HMAC, IP, and UNAUTHENTICATED.
--
-- -   For information about the authentication scheme implemented by
--     GITHUB_HMAC, see
--     <https://developer.github.com/webhooks/securing/ Securing your webhooks>
--     on the GitHub Developer website.
--
-- -   IP rejects webhooks trigger requests unless they originate from an
--     IP address in the IP range whitelisted in the authentication
--     configuration.
--
-- -   UNAUTHENTICATED accepts all webhook trigger requests regardless of
--     origin.
--
-- 'authenticationConfiguration', 'webhookDefinition_authenticationConfiguration' - Properties that configure the authentication applied to incoming webhook
-- trigger requests. The required properties depend on the authentication
-- type. For GITHUB_HMAC, only the @SecretToken @property must be set. For
-- IP, only the @AllowedIPRange @property must be set to a valid CIDR
-- range. For UNAUTHENTICATED, no properties can be set.
newWebhookDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'targetPipeline'
  Prelude.Text ->
  -- | 'targetAction'
  Prelude.Text ->
  -- | 'authentication'
  WebhookAuthenticationType ->
  -- | 'authenticationConfiguration'
  WebhookAuthConfiguration ->
  WebhookDefinition
newWebhookDefinition
  pName_
  pTargetPipeline_
  pTargetAction_
  pAuthentication_
  pAuthenticationConfiguration_ =
    WebhookDefinition'
      { name = pName_,
        targetPipeline = pTargetPipeline_,
        targetAction = pTargetAction_,
        filters = Prelude.mempty,
        authentication = pAuthentication_,
        authenticationConfiguration =
          pAuthenticationConfiguration_
      }

-- | The name of the webhook.
webhookDefinition_name :: Lens.Lens' WebhookDefinition Prelude.Text
webhookDefinition_name = Lens.lens (\WebhookDefinition' {name} -> name) (\s@WebhookDefinition' {} a -> s {name = a} :: WebhookDefinition)

-- | The name of the pipeline you want to connect to the webhook.
webhookDefinition_targetPipeline :: Lens.Lens' WebhookDefinition Prelude.Text
webhookDefinition_targetPipeline = Lens.lens (\WebhookDefinition' {targetPipeline} -> targetPipeline) (\s@WebhookDefinition' {} a -> s {targetPipeline = a} :: WebhookDefinition)

-- | The name of the action in a pipeline you want to connect to the webhook.
-- The action must be from the source (first) stage of the pipeline.
webhookDefinition_targetAction :: Lens.Lens' WebhookDefinition Prelude.Text
webhookDefinition_targetAction = Lens.lens (\WebhookDefinition' {targetAction} -> targetAction) (\s@WebhookDefinition' {} a -> s {targetAction = a} :: WebhookDefinition)

-- | A list of rules applied to the body\/payload sent in the POST request to
-- a webhook URL. All defined rules must pass for the request to be
-- accepted and the pipeline started.
webhookDefinition_filters :: Lens.Lens' WebhookDefinition [WebhookFilterRule]
webhookDefinition_filters = Lens.lens (\WebhookDefinition' {filters} -> filters) (\s@WebhookDefinition' {} a -> s {filters = a} :: WebhookDefinition) Prelude.. Prelude._Coerce

-- | Supported options are GITHUB_HMAC, IP, and UNAUTHENTICATED.
--
-- -   For information about the authentication scheme implemented by
--     GITHUB_HMAC, see
--     <https://developer.github.com/webhooks/securing/ Securing your webhooks>
--     on the GitHub Developer website.
--
-- -   IP rejects webhooks trigger requests unless they originate from an
--     IP address in the IP range whitelisted in the authentication
--     configuration.
--
-- -   UNAUTHENTICATED accepts all webhook trigger requests regardless of
--     origin.
webhookDefinition_authentication :: Lens.Lens' WebhookDefinition WebhookAuthenticationType
webhookDefinition_authentication = Lens.lens (\WebhookDefinition' {authentication} -> authentication) (\s@WebhookDefinition' {} a -> s {authentication = a} :: WebhookDefinition)

-- | Properties that configure the authentication applied to incoming webhook
-- trigger requests. The required properties depend on the authentication
-- type. For GITHUB_HMAC, only the @SecretToken @property must be set. For
-- IP, only the @AllowedIPRange @property must be set to a valid CIDR
-- range. For UNAUTHENTICATED, no properties can be set.
webhookDefinition_authenticationConfiguration :: Lens.Lens' WebhookDefinition WebhookAuthConfiguration
webhookDefinition_authenticationConfiguration = Lens.lens (\WebhookDefinition' {authenticationConfiguration} -> authenticationConfiguration) (\s@WebhookDefinition' {} a -> s {authenticationConfiguration = a} :: WebhookDefinition)

instance Prelude.FromJSON WebhookDefinition where
  parseJSON =
    Prelude.withObject
      "WebhookDefinition"
      ( \x ->
          WebhookDefinition'
            Prelude.<$> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "targetPipeline")
            Prelude.<*> (x Prelude..: "targetAction")
            Prelude.<*> (x Prelude..:? "filters" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..: "authentication")
            Prelude.<*> (x Prelude..: "authenticationConfiguration")
      )

instance Prelude.Hashable WebhookDefinition

instance Prelude.NFData WebhookDefinition

instance Prelude.ToJSON WebhookDefinition where
  toJSON WebhookDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Prelude..= name),
            Prelude.Just
              ("targetPipeline" Prelude..= targetPipeline),
            Prelude.Just
              ("targetAction" Prelude..= targetAction),
            Prelude.Just ("filters" Prelude..= filters),
            Prelude.Just
              ("authentication" Prelude..= authentication),
            Prelude.Just
              ( "authenticationConfiguration"
                  Prelude..= authenticationConfiguration
              )
          ]
      )
