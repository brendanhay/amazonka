{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
  ( Scte35SpliceInsertNoRegionalBlackoutBehavior
    ( Scte35SpliceInsertNoRegionalBlackoutBehavior'
    , Scte35SpliceInsertNoRegionalBlackoutBehaviorFollow
    , Scte35SpliceInsertNoRegionalBlackoutBehaviorIgnore
    , fromScte35SpliceInsertNoRegionalBlackoutBehavior
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Scte35 Splice Insert No Regional Blackout Behavior
newtype Scte35SpliceInsertNoRegionalBlackoutBehavior = Scte35SpliceInsertNoRegionalBlackoutBehavior'{fromScte35SpliceInsertNoRegionalBlackoutBehavior
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

pattern Scte35SpliceInsertNoRegionalBlackoutBehaviorFollow :: Scte35SpliceInsertNoRegionalBlackoutBehavior
pattern Scte35SpliceInsertNoRegionalBlackoutBehaviorFollow = Scte35SpliceInsertNoRegionalBlackoutBehavior' "FOLLOW"

pattern Scte35SpliceInsertNoRegionalBlackoutBehaviorIgnore :: Scte35SpliceInsertNoRegionalBlackoutBehavior
pattern Scte35SpliceInsertNoRegionalBlackoutBehaviorIgnore = Scte35SpliceInsertNoRegionalBlackoutBehavior' "IGNORE"

{-# COMPLETE 
  Scte35SpliceInsertNoRegionalBlackoutBehaviorFollow,

  Scte35SpliceInsertNoRegionalBlackoutBehaviorIgnore,
  Scte35SpliceInsertNoRegionalBlackoutBehavior'
  #-}
