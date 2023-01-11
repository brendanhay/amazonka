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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TopicStatus = TopicStatus'
  { fromTopicStatus ::
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
