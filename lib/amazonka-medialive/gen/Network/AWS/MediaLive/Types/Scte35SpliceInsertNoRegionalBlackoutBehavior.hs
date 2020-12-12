{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Scte35SpliceInsertNoRegionalBlackoutBehavior
  ( Scte35SpliceInsertNoRegionalBlackoutBehavior
      ( Scte35SpliceInsertNoRegionalBlackoutBehavior',
        SSINRBBFollow,
        SSINRBBIgnore
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Scte35 Splice Insert No Regional Blackout Behavior
newtype Scte35SpliceInsertNoRegionalBlackoutBehavior = Scte35SpliceInsertNoRegionalBlackoutBehavior' Lude.Text
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

pattern SSINRBBFollow :: Scte35SpliceInsertNoRegionalBlackoutBehavior
pattern SSINRBBFollow = Scte35SpliceInsertNoRegionalBlackoutBehavior' "FOLLOW"

pattern SSINRBBIgnore :: Scte35SpliceInsertNoRegionalBlackoutBehavior
pattern SSINRBBIgnore = Scte35SpliceInsertNoRegionalBlackoutBehavior' "IGNORE"

{-# COMPLETE
  SSINRBBFollow,
  SSINRBBIgnore,
  Scte35SpliceInsertNoRegionalBlackoutBehavior'
  #-}
