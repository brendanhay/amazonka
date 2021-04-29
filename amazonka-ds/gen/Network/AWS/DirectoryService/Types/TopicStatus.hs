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

import qualified Network.AWS.Prelude as Prelude

newtype TopicStatus = TopicStatus'
  { fromTopicStatus ::
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
