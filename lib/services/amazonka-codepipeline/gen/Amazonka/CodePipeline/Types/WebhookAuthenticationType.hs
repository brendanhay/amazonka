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
-- Module      : Amazonka.CodePipeline.Types.WebhookAuthenticationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.WebhookAuthenticationType
  ( WebhookAuthenticationType
      ( ..,
        WebhookAuthenticationType_GITHUB_HMAC,
        WebhookAuthenticationType_IP,
        WebhookAuthenticationType_UNAUTHENTICATED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WebhookAuthenticationType = WebhookAuthenticationType'
  { fromWebhookAuthenticationType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
