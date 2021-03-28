{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestinationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.TopicRuleDestinationStatus
  ( TopicRuleDestinationStatus
    ( TopicRuleDestinationStatus'
    , TopicRuleDestinationStatusEnabled
    , TopicRuleDestinationStatusInProgress
    , TopicRuleDestinationStatusDisabled
    , TopicRuleDestinationStatusError
    , fromTopicRuleDestinationStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype TopicRuleDestinationStatus = TopicRuleDestinationStatus'{fromTopicRuleDestinationStatus
                                                                 :: Core.Text}
                                       deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                       Core.Generic)
                                       deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                         Core.ToJSONKey, Core.FromJSONKey,
                                                         Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                         Core.FromXML, Core.ToText, Core.FromText,
                                                         Core.ToByteString, Core.ToQuery,
                                                         Core.ToHeader)

pattern TopicRuleDestinationStatusEnabled :: TopicRuleDestinationStatus
pattern TopicRuleDestinationStatusEnabled = TopicRuleDestinationStatus' "ENABLED"

pattern TopicRuleDestinationStatusInProgress :: TopicRuleDestinationStatus
pattern TopicRuleDestinationStatusInProgress = TopicRuleDestinationStatus' "IN_PROGRESS"

pattern TopicRuleDestinationStatusDisabled :: TopicRuleDestinationStatus
pattern TopicRuleDestinationStatusDisabled = TopicRuleDestinationStatus' "DISABLED"

pattern TopicRuleDestinationStatusError :: TopicRuleDestinationStatus
pattern TopicRuleDestinationStatusError = TopicRuleDestinationStatus' "ERROR"

{-# COMPLETE 
  TopicRuleDestinationStatusEnabled,

  TopicRuleDestinationStatusInProgress,

  TopicRuleDestinationStatusDisabled,

  TopicRuleDestinationStatusError,
  TopicRuleDestinationStatus'
  #-}
