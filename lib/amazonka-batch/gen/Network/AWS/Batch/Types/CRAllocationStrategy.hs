{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.CRAllocationStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.CRAllocationStrategy
  ( CRAllocationStrategy
      ( CRAllocationStrategy',
        BestFit,
        BestFitProgressive,
        SpotCapacityOptimized
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype CRAllocationStrategy = CRAllocationStrategy' Lude.Text
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

pattern BestFit :: CRAllocationStrategy
pattern BestFit = CRAllocationStrategy' "BEST_FIT"

pattern BestFitProgressive :: CRAllocationStrategy
pattern BestFitProgressive = CRAllocationStrategy' "BEST_FIT_PROGRESSIVE"

pattern SpotCapacityOptimized :: CRAllocationStrategy
pattern SpotCapacityOptimized = CRAllocationStrategy' "SPOT_CAPACITY_OPTIMIZED"

{-# COMPLETE
  BestFit,
  BestFitProgressive,
  SpotCapacityOptimized,
  CRAllocationStrategy'
  #-}
