{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
  ( Scte35WebDeliveryAllowedFlag
      ( Scte35WebDeliveryAllowedFlag',
        WebDeliveryAllowed,
        WebDeliveryNotAllowed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Corresponds to the web_delivery_allowed_flag parameter. A value of WEB_DELIVERY_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35 specification. If you include one of the "restriction" flags then you must include all four of them.
newtype Scte35WebDeliveryAllowedFlag = Scte35WebDeliveryAllowedFlag' Lude.Text
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

pattern WebDeliveryAllowed :: Scte35WebDeliveryAllowedFlag
pattern WebDeliveryAllowed = Scte35WebDeliveryAllowedFlag' "WEB_DELIVERY_ALLOWED"

pattern WebDeliveryNotAllowed :: Scte35WebDeliveryAllowedFlag
pattern WebDeliveryNotAllowed = Scte35WebDeliveryAllowedFlag' "WEB_DELIVERY_NOT_ALLOWED"

{-# COMPLETE
  WebDeliveryAllowed,
  WebDeliveryNotAllowed,
  Scte35WebDeliveryAllowedFlag'
  #-}
