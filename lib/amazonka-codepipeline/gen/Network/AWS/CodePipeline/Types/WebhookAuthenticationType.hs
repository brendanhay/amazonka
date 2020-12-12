{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookAuthenticationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookAuthenticationType
  ( WebhookAuthenticationType
      ( WebhookAuthenticationType',
        GithubHmac,
        IP,
        Unauthenticated
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype WebhookAuthenticationType = WebhookAuthenticationType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern GithubHmac :: WebhookAuthenticationType
pattern GithubHmac = WebhookAuthenticationType' "GITHUB_HMAC"

pattern IP :: WebhookAuthenticationType
pattern IP = WebhookAuthenticationType' "IP"

pattern Unauthenticated :: WebhookAuthenticationType
pattern Unauthenticated = WebhookAuthenticationType' "UNAUTHENTICATED"

{-# COMPLETE
  GithubHmac,
  IP,
  Unauthenticated,
  WebhookAuthenticationType'
  #-}
