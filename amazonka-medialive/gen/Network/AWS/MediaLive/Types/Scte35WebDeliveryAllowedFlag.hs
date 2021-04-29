{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35WebDeliveryAllowedFlag
  ( Scte35WebDeliveryAllowedFlag
      ( ..,
        Scte35WebDeliveryAllowedFlag_WEB_DELIVERY_ALLOWED,
        Scte35WebDeliveryAllowedFlag_WEB_DELIVERY_NOT_ALLOWED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Corresponds to the web_delivery_allowed_flag parameter. A value of
-- WEB_DELIVERY_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35
-- specification. If you include one of the \"restriction\" flags then you
-- must include all four of them.
newtype Scte35WebDeliveryAllowedFlag = Scte35WebDeliveryAllowedFlag'
  { fromScte35WebDeliveryAllowedFlag ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern Scte35WebDeliveryAllowedFlag_WEB_DELIVERY_ALLOWED :: Scte35WebDeliveryAllowedFlag
pattern Scte35WebDeliveryAllowedFlag_WEB_DELIVERY_ALLOWED = Scte35WebDeliveryAllowedFlag' "WEB_DELIVERY_ALLOWED"

pattern Scte35WebDeliveryAllowedFlag_WEB_DELIVERY_NOT_ALLOWED :: Scte35WebDeliveryAllowedFlag
pattern Scte35WebDeliveryAllowedFlag_WEB_DELIVERY_NOT_ALLOWED = Scte35WebDeliveryAllowedFlag' "WEB_DELIVERY_NOT_ALLOWED"

{-# COMPLETE
  Scte35WebDeliveryAllowedFlag_WEB_DELIVERY_ALLOWED,
  Scte35WebDeliveryAllowedFlag_WEB_DELIVERY_NOT_ALLOWED,
  Scte35WebDeliveryAllowedFlag'
  #-}
