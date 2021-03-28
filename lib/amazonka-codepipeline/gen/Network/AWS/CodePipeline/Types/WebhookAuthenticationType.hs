{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookAuthenticationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.WebhookAuthenticationType
  ( WebhookAuthenticationType
    ( WebhookAuthenticationType'
    , WebhookAuthenticationTypeGithubHmac
    , WebhookAuthenticationTypeIP
    , WebhookAuthenticationTypeUnauthenticated
    , fromWebhookAuthenticationType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype WebhookAuthenticationType = WebhookAuthenticationType'{fromWebhookAuthenticationType
                                                               :: Core.Text}
                                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                      Core.Generic)
                                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                        Core.ToJSONKey, Core.FromJSONKey,
                                                        Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                        Core.FromXML, Core.ToText, Core.FromText,
                                                        Core.ToByteString, Core.ToQuery,
                                                        Core.ToHeader)

pattern WebhookAuthenticationTypeGithubHmac :: WebhookAuthenticationType
pattern WebhookAuthenticationTypeGithubHmac = WebhookAuthenticationType' "GITHUB_HMAC"

pattern WebhookAuthenticationTypeIP :: WebhookAuthenticationType
pattern WebhookAuthenticationTypeIP = WebhookAuthenticationType' "IP"

pattern WebhookAuthenticationTypeUnauthenticated :: WebhookAuthenticationType
pattern WebhookAuthenticationTypeUnauthenticated = WebhookAuthenticationType' "UNAUTHENTICATED"

{-# COMPLETE 
  WebhookAuthenticationTypeGithubHmac,

  WebhookAuthenticationTypeIP,

  WebhookAuthenticationTypeUnauthenticated,
  WebhookAuthenticationType'
  #-}
