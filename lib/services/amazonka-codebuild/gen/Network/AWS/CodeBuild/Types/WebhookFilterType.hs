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
-- Module      : Network.AWS.CodeBuild.Types.WebhookFilterType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.WebhookFilterType
  ( WebhookFilterType
      ( ..,
        WebhookFilterType_ACTOR_ACCOUNT_ID,
        WebhookFilterType_BASE_REF,
        WebhookFilterType_COMMIT_MESSAGE,
        WebhookFilterType_EVENT,
        WebhookFilterType_FILE_PATH,
        WebhookFilterType_HEAD_REF
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype WebhookFilterType = WebhookFilterType'
  { fromWebhookFilterType ::
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

pattern WebhookFilterType_ACTOR_ACCOUNT_ID :: WebhookFilterType
pattern WebhookFilterType_ACTOR_ACCOUNT_ID = WebhookFilterType' "ACTOR_ACCOUNT_ID"

pattern WebhookFilterType_BASE_REF :: WebhookFilterType
pattern WebhookFilterType_BASE_REF = WebhookFilterType' "BASE_REF"

pattern WebhookFilterType_COMMIT_MESSAGE :: WebhookFilterType
pattern WebhookFilterType_COMMIT_MESSAGE = WebhookFilterType' "COMMIT_MESSAGE"

pattern WebhookFilterType_EVENT :: WebhookFilterType
pattern WebhookFilterType_EVENT = WebhookFilterType' "EVENT"

pattern WebhookFilterType_FILE_PATH :: WebhookFilterType
pattern WebhookFilterType_FILE_PATH = WebhookFilterType' "FILE_PATH"

pattern WebhookFilterType_HEAD_REF :: WebhookFilterType
pattern WebhookFilterType_HEAD_REF = WebhookFilterType' "HEAD_REF"

{-# COMPLETE
  WebhookFilterType_ACTOR_ACCOUNT_ID,
  WebhookFilterType_BASE_REF,
  WebhookFilterType_COMMIT_MESSAGE,
  WebhookFilterType_EVENT,
  WebhookFilterType_FILE_PATH,
  WebhookFilterType_HEAD_REF,
  WebhookFilterType'
  #-}
