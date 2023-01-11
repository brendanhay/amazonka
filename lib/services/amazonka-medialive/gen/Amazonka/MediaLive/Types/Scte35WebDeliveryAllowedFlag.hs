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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Corresponds to the web_delivery_allowed_flag parameter. A value of
-- WEB_DELIVERY_NOT_ALLOWED corresponds to 0 (false) in the SCTE-35
-- specification. If you include one of the \"restriction\" flags then you
-- must include all four of them.
newtype Scte35WebDeliveryAllowedFlag = Scte35WebDeliveryAllowedFlag'
  { fromScte35WebDeliveryAllowedFlag ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
