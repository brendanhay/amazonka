{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Kinesis.Types.HashKeyRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kinesis.Types.HashKeyRange
  ( HashKeyRange (..),

    -- * Smart constructor
    mkHashKeyRange,

    -- * Lenses
    hkrStartingHashKey,
    hkrEndingHashKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The range of possible hash key values for the shard, which is a set of ordered contiguous positive integers.
--
-- /See:/ 'mkHashKeyRange' smart constructor.
data HashKeyRange = HashKeyRange'
  { startingHashKey :: Lude.Text,
    endingHashKey :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HashKeyRange' with the minimum fields required to make a request.
--
-- * 'endingHashKey' - The ending hash key of the hash key range.
-- * 'startingHashKey' - The starting hash key of the hash key range.
mkHashKeyRange ::
  -- | 'startingHashKey'
  Lude.Text ->
  -- | 'endingHashKey'
  Lude.Text ->
  HashKeyRange
mkHashKeyRange pStartingHashKey_ pEndingHashKey_ =
  HashKeyRange'
    { startingHashKey = pStartingHashKey_,
      endingHashKey = pEndingHashKey_
    }

-- | The starting hash key of the hash key range.
--
-- /Note:/ Consider using 'startingHashKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkrStartingHashKey :: Lens.Lens' HashKeyRange Lude.Text
hkrStartingHashKey = Lens.lens (startingHashKey :: HashKeyRange -> Lude.Text) (\s a -> s {startingHashKey = a} :: HashKeyRange)
{-# DEPRECATED hkrStartingHashKey "Use generic-lens or generic-optics with 'startingHashKey' instead." #-}

-- | The ending hash key of the hash key range.
--
-- /Note:/ Consider using 'endingHashKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hkrEndingHashKey :: Lens.Lens' HashKeyRange Lude.Text
hkrEndingHashKey = Lens.lens (endingHashKey :: HashKeyRange -> Lude.Text) (\s a -> s {endingHashKey = a} :: HashKeyRange)
{-# DEPRECATED hkrEndingHashKey "Use generic-lens or generic-optics with 'endingHashKey' instead." #-}

instance Lude.FromJSON HashKeyRange where
  parseJSON =
    Lude.withObject
      "HashKeyRange"
      ( \x ->
          HashKeyRange'
            Lude.<$> (x Lude..: "StartingHashKey") Lude.<*> (x Lude..: "EndingHashKey")
      )
