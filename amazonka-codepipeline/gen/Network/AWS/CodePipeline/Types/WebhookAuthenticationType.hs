{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype WebhookAuthenticationType = WebhookAuthenticationType'
  { fromWebhookAuthenticationType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
