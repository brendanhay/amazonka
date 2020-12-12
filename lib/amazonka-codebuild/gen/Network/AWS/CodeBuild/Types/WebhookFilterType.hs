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
        ActorAccountId,
        BaseRef,
        CommitMessage,
        Event,
        FilePath,
        HeadRef
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

pattern ActorAccountId :: WebhookFilterType
pattern ActorAccountId = WebhookFilterType' "ACTOR_ACCOUNT_ID"

pattern BaseRef :: WebhookFilterType
pattern BaseRef = WebhookFilterType' "BASE_REF"

pattern CommitMessage :: WebhookFilterType
pattern CommitMessage = WebhookFilterType' "COMMIT_MESSAGE"

pattern Event :: WebhookFilterType
pattern Event = WebhookFilterType' "EVENT"

pattern FilePath :: WebhookFilterType
pattern FilePath = WebhookFilterType' "FILE_PATH"

pattern HeadRef :: WebhookFilterType
pattern HeadRef = WebhookFilterType' "HEAD_REF"

{-# COMPLETE
  ActorAccountId,
  BaseRef,
  CommitMessage,
  Event,
  FilePath,
  HeadRef,
  WebhookFilterType'
  #-}
