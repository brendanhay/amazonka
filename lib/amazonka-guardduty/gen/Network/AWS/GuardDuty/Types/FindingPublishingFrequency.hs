-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.FindingPublishingFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.FindingPublishingFrequency
  ( FindingPublishingFrequency
      ( FindingPublishingFrequency',
        FifteenMinutes,
        OneHour,
        SixHours
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FindingPublishingFrequency = FindingPublishingFrequency' Lude.Text
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

pattern FifteenMinutes :: FindingPublishingFrequency
pattern FifteenMinutes = FindingPublishingFrequency' "FIFTEEN_MINUTES"

pattern OneHour :: FindingPublishingFrequency
pattern OneHour = FindingPublishingFrequency' "ONE_HOUR"

pattern SixHours :: FindingPublishingFrequency
pattern SixHours = FindingPublishingFrequency' "SIX_HOURS"

{-# COMPLETE
  FifteenMinutes,
  OneHour,
  SixHours,
  FindingPublishingFrequency'
  #-}
