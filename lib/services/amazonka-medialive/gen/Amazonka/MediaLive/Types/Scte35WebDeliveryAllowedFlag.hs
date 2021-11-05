{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MediaLive.Types.Scte35WebDeliveryAllowedFlag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35WebDeliveryAllowedFlag
  ( Scte35WebDeliveryAllowedFlag
      ( ..,
        Scte35WebDeliveryAllowedFlag_WEB_DELIVERY_ALLOWED,
        Scte35WebDeliveryAllowedFlag_WEB_DELIVERY_NOT_ALLOWED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Corresponds to the web_delivery_allowed_flag parameter. A value of
-- WEB_DELIVERY_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35
-- specification. If you include one of the \"restriction\" flags then you
-- must include all four of them.
newtype Scte35WebDeliveryAllowedFlag = Scte35WebDeliveryAllowedFlag'
  { fromScte35WebDeliveryAllowedFlag ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
