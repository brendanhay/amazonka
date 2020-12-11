-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ContributorInsightsStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ContributorInsightsStatus
  ( ContributorInsightsStatus
      ( ContributorInsightsStatus',
        CISDisabled,
        CISDisabling,
        CISEnabled,
        CISEnabling,
        CISFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContributorInsightsStatus = ContributorInsightsStatus' Lude.Text
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

pattern CISDisabled :: ContributorInsightsStatus
pattern CISDisabled = ContributorInsightsStatus' "DISABLED"

pattern CISDisabling :: ContributorInsightsStatus
pattern CISDisabling = ContributorInsightsStatus' "DISABLING"

pattern CISEnabled :: ContributorInsightsStatus
pattern CISEnabled = ContributorInsightsStatus' "ENABLED"

pattern CISEnabling :: ContributorInsightsStatus
pattern CISEnabling = ContributorInsightsStatus' "ENABLING"

pattern CISFailed :: ContributorInsightsStatus
pattern CISFailed = ContributorInsightsStatus' "FAILED"

{-# COMPLETE
  CISDisabled,
  CISDisabling,
  CISEnabled,
  CISEnabling,
  CISFailed,
  ContributorInsightsStatus'
  #-}
