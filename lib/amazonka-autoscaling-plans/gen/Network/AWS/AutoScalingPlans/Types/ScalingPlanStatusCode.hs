{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ScalingPlanStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AutoScalingPlans.Types.ScalingPlanStatusCode
  ( ScalingPlanStatusCode
    ( ScalingPlanStatusCode'
    , ScalingPlanStatusCodeActive
    , ScalingPlanStatusCodeActiveWithProblems
    , ScalingPlanStatusCodeCreationInProgress
    , ScalingPlanStatusCodeCreationFailed
    , ScalingPlanStatusCodeDeletionInProgress
    , ScalingPlanStatusCodeDeletionFailed
    , ScalingPlanStatusCodeUpdateInProgress
    , ScalingPlanStatusCodeUpdateFailed
    , fromScalingPlanStatusCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ScalingPlanStatusCode = ScalingPlanStatusCode'{fromScalingPlanStatusCode
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern ScalingPlanStatusCodeActive :: ScalingPlanStatusCode
pattern ScalingPlanStatusCodeActive = ScalingPlanStatusCode' "Active"

pattern ScalingPlanStatusCodeActiveWithProblems :: ScalingPlanStatusCode
pattern ScalingPlanStatusCodeActiveWithProblems = ScalingPlanStatusCode' "ActiveWithProblems"

pattern ScalingPlanStatusCodeCreationInProgress :: ScalingPlanStatusCode
pattern ScalingPlanStatusCodeCreationInProgress = ScalingPlanStatusCode' "CreationInProgress"

pattern ScalingPlanStatusCodeCreationFailed :: ScalingPlanStatusCode
pattern ScalingPlanStatusCodeCreationFailed = ScalingPlanStatusCode' "CreationFailed"

pattern ScalingPlanStatusCodeDeletionInProgress :: ScalingPlanStatusCode
pattern ScalingPlanStatusCodeDeletionInProgress = ScalingPlanStatusCode' "DeletionInProgress"

pattern ScalingPlanStatusCodeDeletionFailed :: ScalingPlanStatusCode
pattern ScalingPlanStatusCodeDeletionFailed = ScalingPlanStatusCode' "DeletionFailed"

pattern ScalingPlanStatusCodeUpdateInProgress :: ScalingPlanStatusCode
pattern ScalingPlanStatusCodeUpdateInProgress = ScalingPlanStatusCode' "UpdateInProgress"

pattern ScalingPlanStatusCodeUpdateFailed :: ScalingPlanStatusCode
pattern ScalingPlanStatusCodeUpdateFailed = ScalingPlanStatusCode' "UpdateFailed"

{-# COMPLETE 
  ScalingPlanStatusCodeActive,

  ScalingPlanStatusCodeActiveWithProblems,

  ScalingPlanStatusCodeCreationInProgress,

  ScalingPlanStatusCodeCreationFailed,

  ScalingPlanStatusCodeDeletionInProgress,

  ScalingPlanStatusCodeDeletionFailed,

  ScalingPlanStatusCodeUpdateInProgress,

  ScalingPlanStatusCodeUpdateFailed,
  ScalingPlanStatusCode'
  #-}
