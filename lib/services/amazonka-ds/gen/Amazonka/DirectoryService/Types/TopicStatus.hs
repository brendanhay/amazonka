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
-- Module      : Amazonka.DirectoryService.Types.TopicStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.TopicStatus
  ( TopicStatus
      ( ..,
        TopicStatus_Deleted,
        TopicStatus_Failed,
        TopicStatus_Registered,
        TopicStatus_Topic_not_found
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TopicStatus = TopicStatus'
  { fromTopicStatus ::
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
