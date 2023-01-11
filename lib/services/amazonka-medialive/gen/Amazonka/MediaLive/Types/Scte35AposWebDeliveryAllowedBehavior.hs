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
-- Module      : Amazonka.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
  ( Scte35AposWebDeliveryAllowedBehavior
      ( ..,
        Scte35AposWebDeliveryAllowedBehavior_FOLLOW,
        Scte35AposWebDeliveryAllowedBehavior_IGNORE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Scte35 Apos Web Delivery Allowed Behavior
newtype Scte35AposWebDeliveryAllowedBehavior = Scte35AposWebDeliveryAllowedBehavior'
  { fromScte35AposWebDeliveryAllowedBehavior ::
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

pattern Scte35AposWebDeliveryAllowedBehavior_FOLLOW :: Scte35AposWebDeliveryAllowedBehavior
pattern Scte35AposWebDeliveryAllowedBehavior_FOLLOW = Scte35AposWebDeliveryAllowedBehavior' "FOLLOW"

pattern Scte35AposWebDeliveryAllowedBehavior_IGNORE :: Scte35AposWebDeliveryAllowedBehavior
pattern Scte35AposWebDeliveryAllowedBehavior_IGNORE = Scte35AposWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE
  Scte35AposWebDeliveryAllowedBehavior_FOLLOW,
  Scte35AposWebDeliveryAllowedBehavior_IGNORE,
  Scte35AposWebDeliveryAllowedBehavior'
  #-}
