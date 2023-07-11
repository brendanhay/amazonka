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
-- Module      : Amazonka.IoT.Types.TopicRuleDestinationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.TopicRuleDestinationStatus
  ( TopicRuleDestinationStatus
      ( ..,
        TopicRuleDestinationStatus_DELETING,
        TopicRuleDestinationStatus_DISABLED,
        TopicRuleDestinationStatus_ENABLED,
        TopicRuleDestinationStatus_ERROR,
        TopicRuleDestinationStatus_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TopicRuleDestinationStatus = TopicRuleDestinationStatus'
  { fromTopicRuleDestinationStatus ::
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

pattern TopicRuleDestinationStatus_DELETING :: TopicRuleDestinationStatus
pattern TopicRuleDestinationStatus_DELETING = TopicRuleDestinationStatus' "DELETING"

pattern TopicRuleDestinationStatus_DISABLED :: TopicRuleDestinationStatus
pattern TopicRuleDestinationStatus_DISABLED = TopicRuleDestinationStatus' "DISABLED"

pattern TopicRuleDestinationStatus_ENABLED :: TopicRuleDestinationStatus
pattern TopicRuleDestinationStatus_ENABLED = TopicRuleDestinationStatus' "ENABLED"

pattern TopicRuleDestinationStatus_ERROR :: TopicRuleDestinationStatus
pattern TopicRuleDestinationStatus_ERROR = TopicRuleDestinationStatus' "ERROR"

pattern TopicRuleDestinationStatus_IN_PROGRESS :: TopicRuleDestinationStatus
pattern TopicRuleDestinationStatus_IN_PROGRESS = TopicRuleDestinationStatus' "IN_PROGRESS"

{-# COMPLETE
  TopicRuleDestinationStatus_DELETING,
  TopicRuleDestinationStatus_DISABLED,
  TopicRuleDestinationStatus_ENABLED,
  TopicRuleDestinationStatus_ERROR,
  TopicRuleDestinationStatus_IN_PROGRESS,
  TopicRuleDestinationStatus'
  #-}
