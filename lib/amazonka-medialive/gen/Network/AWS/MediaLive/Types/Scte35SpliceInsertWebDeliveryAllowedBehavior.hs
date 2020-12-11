-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
  ( Scte35SpliceInsertWebDeliveryAllowedBehavior
      ( Scte35SpliceInsertWebDeliveryAllowedBehavior',
        SSIWDABFollow,
        SSIWDABIgnore
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Scte35 Splice Insert Web Delivery Allowed Behavior
newtype Scte35SpliceInsertWebDeliveryAllowedBehavior = Scte35SpliceInsertWebDeliveryAllowedBehavior' Lude.Text
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

pattern SSIWDABFollow :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern SSIWDABFollow = Scte35SpliceInsertWebDeliveryAllowedBehavior' "FOLLOW"

pattern SSIWDABIgnore :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern SSIWDABIgnore = Scte35SpliceInsertWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE
  SSIWDABFollow,
  SSIWDABIgnore,
  Scte35SpliceInsertWebDeliveryAllowedBehavior'
  #-}
