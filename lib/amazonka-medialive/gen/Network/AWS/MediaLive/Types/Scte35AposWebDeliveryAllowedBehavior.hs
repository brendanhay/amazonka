{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
  ( Scte35AposWebDeliveryAllowedBehavior
    ( Scte35AposWebDeliveryAllowedBehavior'
    , Scte35AposWebDeliveryAllowedBehaviorFollow
    , Scte35AposWebDeliveryAllowedBehaviorIgnore
    , fromScte35AposWebDeliveryAllowedBehavior
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Scte35 Apos Web Delivery Allowed Behavior
newtype Scte35AposWebDeliveryAllowedBehavior = Scte35AposWebDeliveryAllowedBehavior'{fromScte35AposWebDeliveryAllowedBehavior
                                                                                     :: Core.Text}
                                                 deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                 Core.Show, Core.Generic)
                                                 deriving newtype (Core.IsString, Core.Hashable,
                                                                   Core.NFData, Core.ToJSONKey,
                                                                   Core.FromJSONKey, Core.ToJSON,
                                                                   Core.FromJSON, Core.ToXML,
                                                                   Core.FromXML, Core.ToText,
                                                                   Core.FromText, Core.ToByteString,
                                                                   Core.ToQuery, Core.ToHeader)

pattern Scte35AposWebDeliveryAllowedBehaviorFollow :: Scte35AposWebDeliveryAllowedBehavior
pattern Scte35AposWebDeliveryAllowedBehaviorFollow = Scte35AposWebDeliveryAllowedBehavior' "FOLLOW"

pattern Scte35AposWebDeliveryAllowedBehaviorIgnore :: Scte35AposWebDeliveryAllowedBehavior
pattern Scte35AposWebDeliveryAllowedBehaviorIgnore = Scte35AposWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE 
  Scte35AposWebDeliveryAllowedBehaviorFollow,

  Scte35AposWebDeliveryAllowedBehaviorIgnore,
  Scte35AposWebDeliveryAllowedBehavior'
  #-}
