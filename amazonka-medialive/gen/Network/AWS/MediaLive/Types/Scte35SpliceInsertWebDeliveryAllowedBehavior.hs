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
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
  ( Scte35SpliceInsertWebDeliveryAllowedBehavior
      ( ..,
        Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW,
        Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Scte35 Splice Insert Web Delivery Allowed Behavior
newtype Scte35SpliceInsertWebDeliveryAllowedBehavior = Scte35SpliceInsertWebDeliveryAllowedBehavior'
  { fromScte35SpliceInsertWebDeliveryAllowedBehavior ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW = Scte35SpliceInsertWebDeliveryAllowedBehavior' "FOLLOW"

pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE = Scte35SpliceInsertWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE
  Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW,
  Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE,
  Scte35SpliceInsertWebDeliveryAllowedBehavior'
  #-}
