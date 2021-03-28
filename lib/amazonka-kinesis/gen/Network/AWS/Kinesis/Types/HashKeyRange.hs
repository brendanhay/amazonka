{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.HashKeyRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Kinesis.Types.HashKeyRange
  ( HashKeyRange (..)
  -- * Smart constructor
  , mkHashKeyRange
  -- * Lenses
  , hkrStartingHashKey
  , hkrEndingHashKey
  ) where

import qualified Network.AWS.Kinesis.Types.EndingHashKey as Types
import qualified Network.AWS.Kinesis.Types.StartingHashKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
--
-- /See:/ 'mkHashKeyRange' smart constructor.
data HashKeyRange = HashKeyRange'
  { startingHashKey :: Types.StartingHashKey
    -- ^ The starting hash key of the hash key range.
  , endingHashKey :: Types.EndingHashKey
    -- ^ The ending hash key of the hash key range.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HashKeyRange' value with any optional fields omitted.
mkHashKeyRange
    :: Types.StartingHashKey -- ^ 'startingHashKey'
    -> Types.EndingHashKey -- ^ 'endingHashKey'
    -> HashKeyRange
mkHashKeyRange startingHashKey endingHashKey
  = HashKeyRange'{startingHashKey, endingHashKey}

-- | The starting hash key of the hash key range.
--
-- /Note:/ Consider using 'startingHashKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkrStartingHashKey :: Lens.Lens' HashKeyRange Types.StartingHashKey
hkrStartingHashKey = Lens.field @"startingHashKey"
{-# INLINEABLE hkrStartingHashKey #-}
{-# DEPRECATED startingHashKey "Use generic-lens or generic-optics with 'startingHashKey' instead"  #-}

-- | The ending hash key of the hash key range.
--
-- /Note:/ Consider using 'endingHashKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkrEndingHashKey :: Lens.Lens' HashKeyRange Types.EndingHashKey
hkrEndingHashKey = Lens.field @"endingHashKey"
{-# INLINEABLE hkrEndingHashKey #-}
{-# DEPRECATED endingHashKey "Use generic-lens or generic-optics with 'endingHashKey' instead"  #-}

instance Core.FromJSON HashKeyRange where
        parseJSON
          = Core.withObject "HashKeyRange" Core.$
              \ x ->
                HashKeyRange' Core.<$>
                  (x Core..: "StartingHashKey") Core.<*> x Core..: "EndingHashKey"
