{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ShippingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.ShippingOption
  ( ShippingOption
    ( ShippingOption'
    , ShippingOptionSecondDay
    , ShippingOptionNextDay
    , ShippingOptionExpress
    , ShippingOptionStandard
    , fromShippingOption
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ShippingOption = ShippingOption'{fromShippingOption ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern ShippingOptionSecondDay :: ShippingOption
pattern ShippingOptionSecondDay = ShippingOption' "SECOND_DAY"

pattern ShippingOptionNextDay :: ShippingOption
pattern ShippingOptionNextDay = ShippingOption' "NEXT_DAY"

pattern ShippingOptionExpress :: ShippingOption
pattern ShippingOptionExpress = ShippingOption' "EXPRESS"

pattern ShippingOptionStandard :: ShippingOption
pattern ShippingOptionStandard = ShippingOption' "STANDARD"

{-# COMPLETE 
  ShippingOptionSecondDay,

  ShippingOptionNextDay,

  ShippingOptionExpress,

  ShippingOptionStandard,
  ShippingOption'
  #-}
