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
        TopicStatusRegistered,
        TopicStatusTopicNotFound,
        TopicStatusFailed,
        TopicStatusDeleted,
        fromTopicStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TopicStatus = TopicStatus' {fromTopicStatus :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern TopicStatusRegistered :: TopicStatus
pattern TopicStatusRegistered = TopicStatus' "Registered"

pattern TopicStatusTopicNotFound :: TopicStatus
pattern TopicStatusTopicNotFound = TopicStatus' "Topic not found"

pattern TopicStatusFailed :: TopicStatus
pattern TopicStatusFailed = TopicStatus' "Failed"

pattern TopicStatusDeleted :: TopicStatus
pattern TopicStatusDeleted = TopicStatus' "Deleted"

{-# COMPLETE
  TopicStatusRegistered,
  TopicStatusTopicNotFound,
  TopicStatusFailed,
  TopicStatusDeleted,
  TopicStatus'
  #-}
