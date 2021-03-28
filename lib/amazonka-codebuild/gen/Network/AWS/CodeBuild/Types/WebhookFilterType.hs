{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.WebhookFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.WebhookFilterType
  ( WebhookFilterType
    ( WebhookFilterType'
    , WebhookFilterTypeEvent
    , WebhookFilterTypeBaseRef
    , WebhookFilterTypeHeadRef
    , WebhookFilterTypeActorAccountId
    , WebhookFilterTypeFilePath
    , WebhookFilterTypeCommitMessage
    , fromWebhookFilterType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype WebhookFilterType = WebhookFilterType'{fromWebhookFilterType
                                               :: Core.Text}
                              deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                              Core.Generic)
                              deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                Core.FromJSON, Core.ToXML, Core.FromXML,
                                                Core.ToText, Core.FromText, Core.ToByteString,
                                                Core.ToQuery, Core.ToHeader)

pattern WebhookFilterTypeEvent :: WebhookFilterType
pattern WebhookFilterTypeEvent = WebhookFilterType' "EVENT"

pattern WebhookFilterTypeBaseRef :: WebhookFilterType
pattern WebhookFilterTypeBaseRef = WebhookFilterType' "BASE_REF"

pattern WebhookFilterTypeHeadRef :: WebhookFilterType
pattern WebhookFilterTypeHeadRef = WebhookFilterType' "HEAD_REF"

pattern WebhookFilterTypeActorAccountId :: WebhookFilterType
pattern WebhookFilterTypeActorAccountId = WebhookFilterType' "ACTOR_ACCOUNT_ID"

pattern WebhookFilterTypeFilePath :: WebhookFilterType
pattern WebhookFilterTypeFilePath = WebhookFilterType' "FILE_PATH"

pattern WebhookFilterTypeCommitMessage :: WebhookFilterType
pattern WebhookFilterTypeCommitMessage = WebhookFilterType' "COMMIT_MESSAGE"

{-# COMPLETE 
  WebhookFilterTypeEvent,

  WebhookFilterTypeBaseRef,

  WebhookFilterTypeHeadRef,

  WebhookFilterTypeActorAccountId,

  WebhookFilterTypeFilePath,

  WebhookFilterTypeCommitMessage,
  WebhookFilterType'
  #-}
