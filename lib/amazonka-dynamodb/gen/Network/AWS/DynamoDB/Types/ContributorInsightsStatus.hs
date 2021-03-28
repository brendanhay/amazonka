{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DynamoDB.Types.ContributorInsightsStatus
  ( ContributorInsightsStatus
    ( ContributorInsightsStatus'
    , ContributorInsightsStatusEnabling
    , ContributorInsightsStatusEnabled
    , ContributorInsightsStatusDisabling
    , ContributorInsightsStatusDisabled
    , ContributorInsightsStatusFailed
    , fromContributorInsightsStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ContributorInsightsStatus = ContributorInsightsStatus'{fromContributorInsightsStatus
                                                               :: Core.Text}
                                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                      Core.Generic)
                                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                        Core.ToJSONKey, Core.FromJSONKey,
                                                        Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                        Core.FromXML, Core.ToText, Core.FromText,
                                                        Core.ToByteString, Core.ToQuery,
                                                        Core.ToHeader)

pattern ContributorInsightsStatusEnabling :: ContributorInsightsStatus
pattern ContributorInsightsStatusEnabling = ContributorInsightsStatus' "ENABLING"

pattern ContributorInsightsStatusEnabled :: ContributorInsightsStatus
pattern ContributorInsightsStatusEnabled = ContributorInsightsStatus' "ENABLED"

pattern ContributorInsightsStatusDisabling :: ContributorInsightsStatus
pattern ContributorInsightsStatusDisabling = ContributorInsightsStatus' "DISABLING"

pattern ContributorInsightsStatusDisabled :: ContributorInsightsStatus
pattern ContributorInsightsStatusDisabled = ContributorInsightsStatus' "DISABLED"

pattern ContributorInsightsStatusFailed :: ContributorInsightsStatus
pattern ContributorInsightsStatusFailed = ContributorInsightsStatus' "FAILED"

{-# COMPLETE 
  ContributorInsightsStatusEnabling,

  ContributorInsightsStatusEnabled,

  ContributorInsightsStatusDisabling,

  ContributorInsightsStatusDisabled,

  ContributorInsightsStatusFailed,
  ContributorInsightsStatus'
  #-}
