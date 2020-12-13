{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotAllocationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotAllocationStrategy
  ( SpotAllocationStrategy
      ( SpotAllocationStrategy',
        LowestPrice,
        Diversified,
        CapacityOptimized
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SpotAllocationStrategy = SpotAllocationStrategy' Lude.Text
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

pattern LowestPrice :: SpotAllocationStrategy
pattern LowestPrice = SpotAllocationStrategy' "lowest-price"

pattern Diversified :: SpotAllocationStrategy
pattern Diversified = SpotAllocationStrategy' "diversified"

pattern CapacityOptimized :: SpotAllocationStrategy
pattern CapacityOptimized = SpotAllocationStrategy' "capacity-optimized"

{-# COMPLETE
  LowestPrice,
  Diversified,
  CapacityOptimized,
  SpotAllocationStrategy'
  #-}
