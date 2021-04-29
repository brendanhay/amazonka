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
-- Module      : Network.AWS.CodeBuild.Types.WebhookBuildType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.WebhookBuildType
  ( WebhookBuildType
      ( ..,
        WebhookBuildType_BUILD,
        WebhookBuildType_BUILD_BATCH
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype WebhookBuildType = WebhookBuildType'
  { fromWebhookBuildType ::
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

pattern WebhookBuildType_BUILD :: WebhookBuildType
pattern WebhookBuildType_BUILD = WebhookBuildType' "BUILD"

pattern WebhookBuildType_BUILD_BATCH :: WebhookBuildType
pattern WebhookBuildType_BUILD_BATCH = WebhookBuildType' "BUILD_BATCH"

{-# COMPLETE
  WebhookBuildType_BUILD,
  WebhookBuildType_BUILD_BATCH,
  WebhookBuildType'
  #-}
