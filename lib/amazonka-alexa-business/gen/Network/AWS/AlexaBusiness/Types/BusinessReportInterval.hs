-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportInterval
  ( BusinessReportInterval
      ( BusinessReportInterval',
        OneDay,
        OneWeek,
        ThirtyDays
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype BusinessReportInterval = BusinessReportInterval' Lude.Text
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

pattern OneDay :: BusinessReportInterval
pattern OneDay = BusinessReportInterval' "ONE_DAY"

pattern OneWeek :: BusinessReportInterval
pattern OneWeek = BusinessReportInterval' "ONE_WEEK"

pattern ThirtyDays :: BusinessReportInterval
pattern ThirtyDays = BusinessReportInterval' "THIRTY_DAYS"

{-# COMPLETE
  OneDay,
  OneWeek,
  ThirtyDays,
  BusinessReportInterval'
  #-}
