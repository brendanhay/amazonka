{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.UpgradeStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.UpgradeStatus
  ( UpgradeStatus
      ( UpgradeStatus',
        UpgradeStatusInProgress,
        UpgradeStatusSucceeded,
        UpgradeStatusSucceededWithIssues,
        UpgradeStatusFailed,
        fromUpgradeStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype UpgradeStatus = UpgradeStatus'
  { fromUpgradeStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern UpgradeStatusInProgress :: UpgradeStatus
pattern UpgradeStatusInProgress = UpgradeStatus' "IN_PROGRESS"

pattern UpgradeStatusSucceeded :: UpgradeStatus
pattern UpgradeStatusSucceeded = UpgradeStatus' "SUCCEEDED"

pattern UpgradeStatusSucceededWithIssues :: UpgradeStatus
pattern UpgradeStatusSucceededWithIssues = UpgradeStatus' "SUCCEEDED_WITH_ISSUES"

pattern UpgradeStatusFailed :: UpgradeStatus
pattern UpgradeStatusFailed = UpgradeStatus' "FAILED"

{-# COMPLETE
  UpgradeStatusInProgress,
  UpgradeStatusSucceeded,
  UpgradeStatusSucceededWithIssues,
  UpgradeStatusFailed,
  UpgradeStatus'
  #-}
