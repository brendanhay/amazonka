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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import qualified Amazonka.Prelude as Prelude

-- | Scte35 Apos Web Delivery Allowed Behavior
newtype Scte35AposWebDeliveryAllowedBehavior = Scte35AposWebDeliveryAllowedBehavior'
  { fromScte35AposWebDeliveryAllowedBehavior ::
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

pattern Scte35AposWebDeliveryAllowedBehavior_FOLLOW :: Scte35AposWebDeliveryAllowedBehavior
pattern Scte35AposWebDeliveryAllowedBehavior_FOLLOW = Scte35AposWebDeliveryAllowedBehavior' "FOLLOW"

pattern Scte35AposWebDeliveryAllowedBehavior_IGNORE :: Scte35AposWebDeliveryAllowedBehavior
pattern Scte35AposWebDeliveryAllowedBehavior_IGNORE = Scte35AposWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE
  Scte35AposWebDeliveryAllowedBehavior_FOLLOW,
  Scte35AposWebDeliveryAllowedBehavior_IGNORE,
  Scte35AposWebDeliveryAllowedBehavior'
  #-}
