-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.MaximumExecutionFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.MaximumExecutionFrequency
  ( MaximumExecutionFrequency
      ( MaximumExecutionFrequency',
        OneHour,
        SixHours,
        ThreeHours,
        TwelveHours,
        TwentyFourHours
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MaximumExecutionFrequency = MaximumExecutionFrequency' Lude.Text
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

pattern OneHour :: MaximumExecutionFrequency
pattern OneHour = MaximumExecutionFrequency' "One_Hour"

pattern SixHours :: MaximumExecutionFrequency
pattern SixHours = MaximumExecutionFrequency' "Six_Hours"

pattern ThreeHours :: MaximumExecutionFrequency
pattern ThreeHours = MaximumExecutionFrequency' "Three_Hours"

pattern TwelveHours :: MaximumExecutionFrequency
pattern TwelveHours = MaximumExecutionFrequency' "Twelve_Hours"

pattern TwentyFourHours :: MaximumExecutionFrequency
pattern TwentyFourHours = MaximumExecutionFrequency' "TwentyFour_Hours"

{-# COMPLETE
  OneHour,
  SixHours,
  ThreeHours,
  TwelveHours,
  TwentyFourHours,
  MaximumExecutionFrequency'
  #-}
