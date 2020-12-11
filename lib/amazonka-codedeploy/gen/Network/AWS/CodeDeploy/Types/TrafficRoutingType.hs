-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TrafficRoutingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TrafficRoutingType
  ( TrafficRoutingType
      ( TrafficRoutingType',
        AllAtOnce,
        TimeBasedCanary,
        TimeBasedLinear
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TrafficRoutingType = TrafficRoutingType' Lude.Text
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

pattern AllAtOnce :: TrafficRoutingType
pattern AllAtOnce = TrafficRoutingType' "AllAtOnce"

pattern TimeBasedCanary :: TrafficRoutingType
pattern TimeBasedCanary = TrafficRoutingType' "TimeBasedCanary"

pattern TimeBasedLinear :: TrafficRoutingType
pattern TimeBasedLinear = TrafficRoutingType' "TimeBasedLinear"

{-# COMPLETE
  AllAtOnce,
  TimeBasedCanary,
  TimeBasedLinear,
  TrafficRoutingType'
  #-}
