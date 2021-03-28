{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportInterval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.BusinessReportInterval
  ( BusinessReportInterval
    ( BusinessReportInterval'
    , BusinessReportIntervalOneDay
    , BusinessReportIntervalOneWeek
    , BusinessReportIntervalThirtyDays
    , fromBusinessReportInterval
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype BusinessReportInterval = BusinessReportInterval'{fromBusinessReportInterval
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern BusinessReportIntervalOneDay :: BusinessReportInterval
pattern BusinessReportIntervalOneDay = BusinessReportInterval' "ONE_DAY"

pattern BusinessReportIntervalOneWeek :: BusinessReportInterval
pattern BusinessReportIntervalOneWeek = BusinessReportInterval' "ONE_WEEK"

pattern BusinessReportIntervalThirtyDays :: BusinessReportInterval
pattern BusinessReportIntervalThirtyDays = BusinessReportInterval' "THIRTY_DAYS"

{-# COMPLETE 
  BusinessReportIntervalOneDay,

  BusinessReportIntervalOneWeek,

  BusinessReportIntervalThirtyDays,
  BusinessReportInterval'
  #-}
