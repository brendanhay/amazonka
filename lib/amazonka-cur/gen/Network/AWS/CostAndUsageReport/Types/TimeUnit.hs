-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.TimeUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.TimeUnit
  ( TimeUnit
      ( TimeUnit',
        Daily,
        Hourly,
        Monthly
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The length of time covered by the report.
newtype TimeUnit = TimeUnit' Lude.Text
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

pattern Daily :: TimeUnit
pattern Daily = TimeUnit' "DAILY"

pattern Hourly :: TimeUnit
pattern Hourly = TimeUnit' "HOURLY"

pattern Monthly :: TimeUnit
pattern Monthly = TimeUnit' "MONTHLY"

{-# COMPLETE
  Daily,
  Hourly,
  Monthly,
  TimeUnit'
  #-}
