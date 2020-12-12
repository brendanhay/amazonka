{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TopicRuleDestinationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TopicRuleDestinationStatus
  ( TopicRuleDestinationStatus
      ( TopicRuleDestinationStatus',
        TRDSDisabled,
        TRDSEnabled,
        TRDSError,
        TRDSInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TopicRuleDestinationStatus = TopicRuleDestinationStatus' Lude.Text
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

pattern TRDSDisabled :: TopicRuleDestinationStatus
pattern TRDSDisabled = TopicRuleDestinationStatus' "DISABLED"

pattern TRDSEnabled :: TopicRuleDestinationStatus
pattern TRDSEnabled = TopicRuleDestinationStatus' "ENABLED"

pattern TRDSError :: TopicRuleDestinationStatus
pattern TRDSError = TopicRuleDestinationStatus' "ERROR"

pattern TRDSInProgress :: TopicRuleDestinationStatus
pattern TRDSInProgress = TopicRuleDestinationStatus' "IN_PROGRESS"

{-# COMPLETE
  TRDSDisabled,
  TRDSEnabled,
  TRDSError,
  TRDSInProgress,
  TopicRuleDestinationStatus'
  #-}
