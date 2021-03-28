{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.ProactiveEngagementStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.ProactiveEngagementStatus
  ( ProactiveEngagementStatus
    ( ProactiveEngagementStatus'
    , ProactiveEngagementStatusEnabled
    , ProactiveEngagementStatusDisabled
    , ProactiveEngagementStatusPending
    , fromProactiveEngagementStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ProactiveEngagementStatus = ProactiveEngagementStatus'{fromProactiveEngagementStatus
                                                               :: Core.Text}
                                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                      Core.Generic)
                                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                        Core.ToJSONKey, Core.FromJSONKey,
                                                        Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                        Core.FromXML, Core.ToText, Core.FromText,
                                                        Core.ToByteString, Core.ToQuery,
                                                        Core.ToHeader)

pattern ProactiveEngagementStatusEnabled :: ProactiveEngagementStatus
pattern ProactiveEngagementStatusEnabled = ProactiveEngagementStatus' "ENABLED"

pattern ProactiveEngagementStatusDisabled :: ProactiveEngagementStatus
pattern ProactiveEngagementStatusDisabled = ProactiveEngagementStatus' "DISABLED"

pattern ProactiveEngagementStatusPending :: ProactiveEngagementStatus
pattern ProactiveEngagementStatusPending = ProactiveEngagementStatus' "PENDING"

{-# COMPLETE 
  ProactiveEngagementStatusEnabled,

  ProactiveEngagementStatusDisabled,

  ProactiveEngagementStatusPending,
  ProactiveEngagementStatus'
  #-}
