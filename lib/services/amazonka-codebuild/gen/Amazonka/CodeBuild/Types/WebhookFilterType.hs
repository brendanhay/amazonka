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
-- Module      : Amazonka.CodeBuild.Types.WebhookFilterType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.WebhookFilterType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype WebhookFilterType = WebhookFilterType'
  { fromWebhookFilterType ::
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
