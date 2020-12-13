{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        CISEnabling,
        CISEnabled,
        CISDisabling,
        CISDisabled,
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

pattern CISEnabling :: ContributorInsightsStatus
pattern CISEnabling = ContributorInsightsStatus' "ENABLING"

pattern CISEnabled :: ContributorInsightsStatus
pattern CISEnabled = ContributorInsightsStatus' "ENABLED"

pattern CISDisabling :: ContributorInsightsStatus
pattern CISDisabling = ContributorInsightsStatus' "DISABLING"

pattern CISDisabled :: ContributorInsightsStatus
pattern CISDisabled = ContributorInsightsStatus' "DISABLED"

pattern CISFailed :: ContributorInsightsStatus
pattern CISFailed = ContributorInsightsStatus' "FAILED"

{-# COMPLETE
  CISEnabling,
  CISEnabled,
  CISDisabling,
  CISDisabled,
  CISFailed,
  ContributorInsightsStatus'
  #-}
