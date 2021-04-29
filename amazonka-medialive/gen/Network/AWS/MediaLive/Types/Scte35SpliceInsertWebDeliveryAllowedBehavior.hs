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

import qualified Network.AWS.Prelude as Prelude

-- | Scte35 Splice Insert Web Delivery Allowed Behavior
newtype Scte35SpliceInsertWebDeliveryAllowedBehavior = Scte35SpliceInsertWebDeliveryAllowedBehavior'
  { fromScte35SpliceInsertWebDeliveryAllowedBehavior ::
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

pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW = Scte35SpliceInsertWebDeliveryAllowedBehavior' "FOLLOW"

pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE = Scte35SpliceInsertWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE
  Scte35SpliceInsertWebDeliveryAllowedBehavior_FOLLOW,
  Scte35SpliceInsertWebDeliveryAllowedBehavior_IGNORE,
  Scte35SpliceInsertWebDeliveryAllowedBehavior'
  #-}
