{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.MaximumExecutionFrequency
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Config.Types.MaximumExecutionFrequency
  ( MaximumExecutionFrequency
    ( MaximumExecutionFrequency'
    , MaximumExecutionFrequencyOneHour
    , MaximumExecutionFrequencyThreeHours
    , MaximumExecutionFrequencySixHours
    , MaximumExecutionFrequencyTwelveHours
    , MaximumExecutionFrequencyTwentyFourHours
    , fromMaximumExecutionFrequency
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype MaximumExecutionFrequency = MaximumExecutionFrequency'{fromMaximumExecutionFrequency
                                                               :: Core.Text}
                                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                      Core.Generic)
                                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                        Core.ToJSONKey, Core.FromJSONKey,
                                                        Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                        Core.FromXML, Core.ToText, Core.FromText,
                                                        Core.ToByteString, Core.ToQuery,
                                                        Core.ToHeader)

pattern MaximumExecutionFrequencyOneHour :: MaximumExecutionFrequency
pattern MaximumExecutionFrequencyOneHour = MaximumExecutionFrequency' "One_Hour"

pattern MaximumExecutionFrequencyThreeHours :: MaximumExecutionFrequency
pattern MaximumExecutionFrequencyThreeHours = MaximumExecutionFrequency' "Three_Hours"

pattern MaximumExecutionFrequencySixHours :: MaximumExecutionFrequency
pattern MaximumExecutionFrequencySixHours = MaximumExecutionFrequency' "Six_Hours"

pattern MaximumExecutionFrequencyTwelveHours :: MaximumExecutionFrequency
pattern MaximumExecutionFrequencyTwelveHours = MaximumExecutionFrequency' "Twelve_Hours"

pattern MaximumExecutionFrequencyTwentyFourHours :: MaximumExecutionFrequency
pattern MaximumExecutionFrequencyTwentyFourHours = MaximumExecutionFrequency' "TwentyFour_Hours"

{-# COMPLETE 
  MaximumExecutionFrequencyOneHour,

  MaximumExecutionFrequencyThreeHours,

  MaximumExecutionFrequencySixHours,

  MaximumExecutionFrequencyTwelveHours,

  MaximumExecutionFrequencyTwentyFourHours,
  MaximumExecutionFrequency'
  #-}
