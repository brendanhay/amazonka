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
-- Module      : Network.AWS.DirectoryService.Types.TopicStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.TopicStatus
  ( TopicStatus
      ( ..,
        TopicStatus_Deleted,
        TopicStatus_Failed,
        TopicStatus_Registered,
        TopicStatus_Topic_not_found
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TopicStatus = TopicStatus'
  { fromTopicStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern TopicStatus_Deleted :: TopicStatus
pattern TopicStatus_Deleted = TopicStatus' "Deleted"

pattern TopicStatus_Failed :: TopicStatus
pattern TopicStatus_Failed = TopicStatus' "Failed"

pattern TopicStatus_Registered :: TopicStatus
pattern TopicStatus_Registered = TopicStatus' "Registered"

pattern TopicStatus_Topic_not_found :: TopicStatus
pattern TopicStatus_Topic_not_found = TopicStatus' "Topic not found"

{-# COMPLETE
  TopicStatus_Deleted,
  TopicStatus_Failed,
  TopicStatus_Registered,
  TopicStatus_Topic_not_found,
  TopicStatus'
  #-}
