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
-- Module      : Network.AWS.IoT.Types.TopicRuleDestinationStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestinationStatus
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

import qualified Network.AWS.Prelude as Prelude

newtype TopicRuleDestinationStatus = TopicRuleDestinationStatus'
  { fromTopicRuleDestinationStatus ::
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
