{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
  ( Scte35WebDeliveryAllowedFlag
    ( Scte35WebDeliveryAllowedFlag'
    , Scte35WebDeliveryAllowedFlagWebDeliveryNotAllowed
    , Scte35WebDeliveryAllowedFlagWebDeliveryAllowed
    , fromScte35WebDeliveryAllowedFlag
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Corresponds to the web_delivery_allowed_flag parameter. A value of WEB_DELIVERY_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
newtype Scte35WebDeliveryAllowedFlag = Scte35WebDeliveryAllowedFlag'{fromScte35WebDeliveryAllowedFlag
                                                                     :: Core.Text}
                                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                         Core.Generic)
                                         deriving newtype (Core.IsString, Core.Hashable,
                                                           Core.NFData, Core.ToJSONKey,
                                                           Core.FromJSONKey, Core.ToJSON,
                                                           Core.FromJSON, Core.ToXML, Core.FromXML,
                                                           Core.ToText, Core.FromText,
                                                           Core.ToByteString, Core.ToQuery,
                                                           Core.ToHeader)

pattern Scte35WebDeliveryAllowedFlagWebDeliveryNotAllowed :: Scte35WebDeliveryAllowedFlag
pattern Scte35WebDeliveryAllowedFlagWebDeliveryNotAllowed = Scte35WebDeliveryAllowedFlag' "WEB_DELIVERY_NOT_ALLOWED"

pattern Scte35WebDeliveryAllowedFlagWebDeliveryAllowed :: Scte35WebDeliveryAllowedFlag
pattern Scte35WebDeliveryAllowedFlagWebDeliveryAllowed = Scte35WebDeliveryAllowedFlag' "WEB_DELIVERY_ALLOWED"

{-# COMPLETE 
  Scte35WebDeliveryAllowedFlagWebDeliveryNotAllowed,

  Scte35WebDeliveryAllowedFlagWebDeliveryAllowed,
  Scte35WebDeliveryAllowedFlag'
  #-}
