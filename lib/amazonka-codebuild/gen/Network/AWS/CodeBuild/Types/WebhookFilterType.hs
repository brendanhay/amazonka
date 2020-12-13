{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.WebhookFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.WebhookFilterType
  ( WebhookFilterType
      ( WebhookFilterType',
        Event,
        BaseRef,
        HeadRef,
        ActorAccountId,
        FilePath,
        CommitMessage
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype WebhookFilterType = WebhookFilterType' Lude.Text
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

pattern Event :: WebhookFilterType
pattern Event = WebhookFilterType' "EVENT"

pattern BaseRef :: WebhookFilterType
pattern BaseRef = WebhookFilterType' "BASE_REF"

pattern HeadRef :: WebhookFilterType
pattern HeadRef = WebhookFilterType' "HEAD_REF"

pattern ActorAccountId :: WebhookFilterType
pattern ActorAccountId = WebhookFilterType' "ACTOR_ACCOUNT_ID"

pattern FilePath :: WebhookFilterType
pattern FilePath = WebhookFilterType' "FILE_PATH"

pattern CommitMessage :: WebhookFilterType
pattern CommitMessage = WebhookFilterType' "COMMIT_MESSAGE"

{-# COMPLETE
  Event,
  BaseRef,
  HeadRef,
  ActorAccountId,
  FilePath,
  CommitMessage,
  WebhookFilterType'
  #-}
