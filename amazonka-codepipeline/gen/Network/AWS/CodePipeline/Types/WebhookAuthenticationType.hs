{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.WebhookAuthenticationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.WebhookAuthenticationType
  ( WebhookAuthenticationType
      ( ..,
        WebhookAuthenticationType_GITHUB_HMAC,
        WebhookAuthenticationType_IP,
        WebhookAuthenticationType_UNAUTHENTICATED
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype WebhookAuthenticationType = WebhookAuthenticationType'
  { fromWebhookAuthenticationType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern WebhookAuthenticationType_GITHUB_HMAC :: WebhookAuthenticationType
pattern WebhookAuthenticationType_GITHUB_HMAC = WebhookAuthenticationType' "GITHUB_HMAC"

pattern WebhookAuthenticationType_IP :: WebhookAuthenticationType
pattern WebhookAuthenticationType_IP = WebhookAuthenticationType' "IP"

pattern WebhookAuthenticationType_UNAUTHENTICATED :: WebhookAuthenticationType
pattern WebhookAuthenticationType_UNAUTHENTICATED = WebhookAuthenticationType' "UNAUTHENTICATED"

{-# COMPLETE
  WebhookAuthenticationType_GITHUB_HMAC,
  WebhookAuthenticationType_IP,
  WebhookAuthenticationType_UNAUTHENTICATED,
  WebhookAuthenticationType'
  #-}
