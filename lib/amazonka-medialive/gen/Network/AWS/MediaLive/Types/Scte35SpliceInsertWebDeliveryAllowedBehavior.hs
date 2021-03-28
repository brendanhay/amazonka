{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35SpliceInsertWebDeliveryAllowedBehavior
  ( Scte35SpliceInsertWebDeliveryAllowedBehavior
    ( Scte35SpliceInsertWebDeliveryAllowedBehavior'
    , Scte35SpliceInsertWebDeliveryAllowedBehaviorFollow
    , Scte35SpliceInsertWebDeliveryAllowedBehaviorIgnore
    , fromScte35SpliceInsertWebDeliveryAllowedBehavior
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Scte35 Splice Insert Web Delivery Allowed Behavior
newtype Scte35SpliceInsertWebDeliveryAllowedBehavior = Scte35SpliceInsertWebDeliveryAllowedBehavior'{fromScte35SpliceInsertWebDeliveryAllowedBehavior
                                                                                                     ::
                                                                                                     Core.Text}
                                                         deriving stock (Core.Eq, Core.Ord,
                                                                         Core.Read, Core.Show,
                                                                         Core.Generic)
                                                         deriving newtype (Core.IsString,
                                                                           Core.Hashable,
                                                                           Core.NFData,
                                                                           Core.ToJSONKey,
                                                                           Core.FromJSONKey,
                                                                           Core.ToJSON,
                                                                           Core.FromJSON,
                                                                           Core.ToXML, Core.FromXML,
                                                                           Core.ToText,
                                                                           Core.FromText,
                                                                           Core.ToByteString,
                                                                           Core.ToQuery,
                                                                           Core.ToHeader)

pattern Scte35SpliceInsertWebDeliveryAllowedBehaviorFollow :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern Scte35SpliceInsertWebDeliveryAllowedBehaviorFollow = Scte35SpliceInsertWebDeliveryAllowedBehavior' "FOLLOW"

pattern Scte35SpliceInsertWebDeliveryAllowedBehaviorIgnore :: Scte35SpliceInsertWebDeliveryAllowedBehavior
pattern Scte35SpliceInsertWebDeliveryAllowedBehaviorIgnore = Scte35SpliceInsertWebDeliveryAllowedBehavior' "IGNORE"

{-# COMPLETE 
  Scte35SpliceInsertWebDeliveryAllowedBehaviorFollow,

  Scte35SpliceInsertWebDeliveryAllowedBehaviorIgnore,
  Scte35SpliceInsertWebDeliveryAllowedBehavior'
  #-}
