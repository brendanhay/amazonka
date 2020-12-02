{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookDefinition where

import Network.AWS.CodePipeline.Types.WebhookAuthConfiguration
import Network.AWS.CodePipeline.Types.WebhookAuthenticationType
import Network.AWS.CodePipeline.Types.WebhookFilterRule
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about a webhook and its definition.
--
--
--
-- /See:/ 'webhookDefinition' smart constructor.
data WebhookDefinition = WebhookDefinition'
  { _wdName :: !Text,
    _wdTargetPipeline :: !Text,
    _wdTargetAction :: !Text,
    _wdFilters :: ![WebhookFilterRule],
    _wdAuthentication :: !WebhookAuthenticationType,
    _wdAuthenticationConfiguration ::
      !WebhookAuthConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WebhookDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wdName' - The name of the webhook.
--
-- * 'wdTargetPipeline' - The name of the pipeline you want to connect to the webhook.
--
-- * 'wdTargetAction' - The name of the action in a pipeline you want to connect to the webhook. The action must be from the source (first) stage of the pipeline.
--
-- * 'wdFilters' - A list of rules applied to the body/payload sent in the POST request to a webhook URL. All defined rules must pass for the request to be accepted and the pipeline started.
--
-- * 'wdAuthentication' - Supported options are GITHUB_HMAC, IP, and UNAUTHENTICATED.     * For information about the authentication scheme implemented by GITHUB_HMAC, see <https://developer.github.com/webhooks/securing/ Securing your webhooks> on the GitHub Developer website.     * IP rejects webhooks trigger requests unless they originate from an IP address in the IP range whitelisted in the authentication configuration.     * UNAUTHENTICATED accepts all webhook trigger requests regardless of origin.
--
-- * 'wdAuthenticationConfiguration' - Properties that configure the authentication applied to incoming webhook trigger requests. The required properties depend on the authentication type. For GITHUB_HMAC, only the @SecretToken @ property must be set. For IP, only the @AllowedIPRange @ property must be set to a valid CIDR range. For UNAUTHENTICATED, no properties can be set.
webhookDefinition ::
  -- | 'wdName'
  Text ->
  -- | 'wdTargetPipeline'
  Text ->
  -- | 'wdTargetAction'
  Text ->
  -- | 'wdAuthentication'
  WebhookAuthenticationType ->
  -- | 'wdAuthenticationConfiguration'
  WebhookAuthConfiguration ->
  WebhookDefinition
webhookDefinition
  pName_
  pTargetPipeline_
  pTargetAction_
  pAuthentication_
  pAuthenticationConfiguration_ =
    WebhookDefinition'
      { _wdName = pName_,
        _wdTargetPipeline = pTargetPipeline_,
        _wdTargetAction = pTargetAction_,
        _wdFilters = mempty,
        _wdAuthentication = pAuthentication_,
        _wdAuthenticationConfiguration = pAuthenticationConfiguration_
      }

-- | The name of the webhook.
wdName :: Lens' WebhookDefinition Text
wdName = lens _wdName (\s a -> s {_wdName = a})

-- | The name of the pipeline you want to connect to the webhook.
wdTargetPipeline :: Lens' WebhookDefinition Text
wdTargetPipeline = lens _wdTargetPipeline (\s a -> s {_wdTargetPipeline = a})

-- | The name of the action in a pipeline you want to connect to the webhook. The action must be from the source (first) stage of the pipeline.
wdTargetAction :: Lens' WebhookDefinition Text
wdTargetAction = lens _wdTargetAction (\s a -> s {_wdTargetAction = a})

-- | A list of rules applied to the body/payload sent in the POST request to a webhook URL. All defined rules must pass for the request to be accepted and the pipeline started.
wdFilters :: Lens' WebhookDefinition [WebhookFilterRule]
wdFilters = lens _wdFilters (\s a -> s {_wdFilters = a}) . _Coerce

-- | Supported options are GITHUB_HMAC, IP, and UNAUTHENTICATED.     * For information about the authentication scheme implemented by GITHUB_HMAC, see <https://developer.github.com/webhooks/securing/ Securing your webhooks> on the GitHub Developer website.     * IP rejects webhooks trigger requests unless they originate from an IP address in the IP range whitelisted in the authentication configuration.     * UNAUTHENTICATED accepts all webhook trigger requests regardless of origin.
wdAuthentication :: Lens' WebhookDefinition WebhookAuthenticationType
wdAuthentication = lens _wdAuthentication (\s a -> s {_wdAuthentication = a})

-- | Properties that configure the authentication applied to incoming webhook trigger requests. The required properties depend on the authentication type. For GITHUB_HMAC, only the @SecretToken @ property must be set. For IP, only the @AllowedIPRange @ property must be set to a valid CIDR range. For UNAUTHENTICATED, no properties can be set.
wdAuthenticationConfiguration :: Lens' WebhookDefinition WebhookAuthConfiguration
wdAuthenticationConfiguration = lens _wdAuthenticationConfiguration (\s a -> s {_wdAuthenticationConfiguration = a})

instance FromJSON WebhookDefinition where
  parseJSON =
    withObject
      "WebhookDefinition"
      ( \x ->
          WebhookDefinition'
            <$> (x .: "name")
            <*> (x .: "targetPipeline")
            <*> (x .: "targetAction")
            <*> (x .:? "filters" .!= mempty)
            <*> (x .: "authentication")
            <*> (x .: "authenticationConfiguration")
      )

instance Hashable WebhookDefinition

instance NFData WebhookDefinition

instance ToJSON WebhookDefinition where
  toJSON WebhookDefinition' {..} =
    object
      ( catMaybes
          [ Just ("name" .= _wdName),
            Just ("targetPipeline" .= _wdTargetPipeline),
            Just ("targetAction" .= _wdTargetAction),
            Just ("filters" .= _wdFilters),
            Just ("authentication" .= _wdAuthentication),
            Just
              ("authenticationConfiguration" .= _wdAuthenticationConfiguration)
          ]
      )
