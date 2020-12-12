{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.TopicStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.TopicStatus
  ( TopicStatus
      ( TopicStatus',
        TDeleted,
        TFailed,
        TRegistered,
        TTopicNotFound
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TopicStatus = TopicStatus' Lude.Text
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

pattern TDeleted :: TopicStatus
pattern TDeleted = TopicStatus' "Deleted"

pattern TFailed :: TopicStatus
pattern TFailed = TopicStatus' "Failed"

pattern TRegistered :: TopicStatus
pattern TRegistered = TopicStatus' "Registered"

pattern TTopicNotFound :: TopicStatus
pattern TTopicNotFound = TopicStatus' "Topic not found"

{-# COMPLETE
  TDeleted,
  TFailed,
  TRegistered,
  TTopicNotFound,
  TopicStatus'
  #-}
