{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackSetOperationResultStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackSetOperationResultStatus
  ( StackSetOperationResultStatus
    ( StackSetOperationResultStatus'
    , StackSetOperationResultStatusPending
    , StackSetOperationResultStatusRunning
    , StackSetOperationResultStatusSucceeded
    , StackSetOperationResultStatusFailed
    , StackSetOperationResultStatusCancelled
    , fromStackSetOperationResultStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype StackSetOperationResultStatus = StackSetOperationResultStatus'{fromStackSetOperationResultStatus
                                                                       :: Core.Text}
                                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                          Core.Generic)
                                          deriving newtype (Core.IsString, Core.Hashable,
                                                            Core.NFData, Core.ToJSONKey,
                                                            Core.FromJSONKey, Core.ToJSON,
                                                            Core.FromJSON, Core.ToXML, Core.FromXML,
                                                            Core.ToText, Core.FromText,
                                                            Core.ToByteString, Core.ToQuery,
                                                            Core.ToHeader)

pattern StackSetOperationResultStatusPending :: StackSetOperationResultStatus
pattern StackSetOperationResultStatusPending = StackSetOperationResultStatus' "PENDING"

pattern StackSetOperationResultStatusRunning :: StackSetOperationResultStatus
pattern StackSetOperationResultStatusRunning = StackSetOperationResultStatus' "RUNNING"

pattern StackSetOperationResultStatusSucceeded :: StackSetOperationResultStatus
pattern StackSetOperationResultStatusSucceeded = StackSetOperationResultStatus' "SUCCEEDED"

pattern StackSetOperationResultStatusFailed :: StackSetOperationResultStatus
pattern StackSetOperationResultStatusFailed = StackSetOperationResultStatus' "FAILED"

pattern StackSetOperationResultStatusCancelled :: StackSetOperationResultStatus
pattern StackSetOperationResultStatusCancelled = StackSetOperationResultStatus' "CANCELLED"

{-# COMPLETE 
  StackSetOperationResultStatusPending,

  StackSetOperationResultStatusRunning,

  StackSetOperationResultStatusSucceeded,

  StackSetOperationResultStatusFailed,

  StackSetOperationResultStatusCancelled,
  StackSetOperationResultStatus'
  #-}
