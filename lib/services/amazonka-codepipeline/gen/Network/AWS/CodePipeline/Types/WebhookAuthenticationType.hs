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
import qualified Network.AWS.Prelude as Prelude

newtype WebhookAuthenticationType = WebhookAuthenticationType'
  { fromWebhookAuthenticationType ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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
