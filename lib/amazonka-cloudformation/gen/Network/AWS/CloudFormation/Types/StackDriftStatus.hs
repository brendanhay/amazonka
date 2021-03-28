{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackDriftStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackDriftStatus
  ( StackDriftStatus
    ( StackDriftStatus'
    , StackDriftStatusDrifted
    , StackDriftStatusInSync
    , StackDriftStatusUnknown
    , StackDriftStatusNotChecked
    , fromStackDriftStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype StackDriftStatus = StackDriftStatus'{fromStackDriftStatus
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern StackDriftStatusDrifted :: StackDriftStatus
pattern StackDriftStatusDrifted = StackDriftStatus' "DRIFTED"

pattern StackDriftStatusInSync :: StackDriftStatus
pattern StackDriftStatusInSync = StackDriftStatus' "IN_SYNC"

pattern StackDriftStatusUnknown :: StackDriftStatus
pattern StackDriftStatusUnknown = StackDriftStatus' "UNKNOWN"

pattern StackDriftStatusNotChecked :: StackDriftStatus
pattern StackDriftStatusNotChecked = StackDriftStatus' "NOT_CHECKED"

{-# COMPLETE 
  StackDriftStatusDrifted,

  StackDriftStatusInSync,

  StackDriftStatusUnknown,

  StackDriftStatusNotChecked,
  StackDriftStatus'
  #-}
