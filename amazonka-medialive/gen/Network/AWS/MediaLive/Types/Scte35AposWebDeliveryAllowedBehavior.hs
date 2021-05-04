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
-- Module      : Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35AposWebDeliveryAllowedBehavior
  ( Scte35AposWebDeliveryAllowedBehavior
      ( ..,
        Scte35AposWebDeliveryAllowedBehavior_FOLLOW,
        Scte35AposWebDeliveryAllowedBehavior_IGNORE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Scte35 Apos Web Delivery Allowed Behavior
newtype Scte35AposWebDeliveryAllowedBehavior = Scte35AposWebDeliveryAllowedBehavior'
  { fromScte35AposWebDeliveryAllowedBehavior ::
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

pattern Scte35AposWebDeliveryAllowedBehavior_FOLLOW :: Scte35AposWebDeliveryAllowedBehavior
pattern Scte35AposWebDeliveryAllowedBehavior_FOLLOW = Scte35AposWebDeliveryAllowedBehavior' "FOLLOW"

pattern Scte35AposWebDeliveryAllowedBehavior_IGNORE :: Scte35AposWebDeliveryAllowedBehavior
pattern Scte35AposWebDeliveryAllowedBehavior_IGNORE = Scte35AposWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE
  Scte35AposWebDeliveryAllowedBehavior_FOLLOW,
  Scte35AposWebDeliveryAllowedBehavior_IGNORE,
  Scte35AposWebDeliveryAllowedBehavior'
  #-}
