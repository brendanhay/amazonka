{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.MergeOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.MergeOperations
  ( MergeOperations (..),

    -- * Smart constructor
    mkMergeOperations,

    -- * Lenses
    moDestination,
    moSource,
  )
where

import Network.AWS.CodeCommit.Types.ChangeTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the file operation conflicts in a merge operation.
--
-- /See:/ 'mkMergeOperations' smart constructor.
data MergeOperations = MergeOperations'
  { destination ::
      Lude.Maybe ChangeTypeEnum,
    source :: Lude.Maybe ChangeTypeEnum
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MergeOperations' with the minimum fields required to make a request.
--
-- * 'destination' - The operation on a file in the destination of a merge or pull request.
-- * 'source' - The operation (add, modify, or delete) on a file in the source of a merge or pull request.
mkMergeOperations ::
  MergeOperations
mkMergeOperations =
  MergeOperations'
    { destination = Lude.Nothing,
      source = Lude.Nothing
    }

-- | The operation on a file in the destination of a merge or pull request.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
moDestination :: Lens.Lens' MergeOperations (Lude.Maybe ChangeTypeEnum)
moDestination = Lens.lens (destination :: MergeOperations -> Lude.Maybe ChangeTypeEnum) (\s a -> s {destination = a} :: MergeOperations)
{-# DEPRECATED moDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

-- | The operation (add, modify, or delete) on a file in the source of a merge or pull request.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
moSource :: Lens.Lens' MergeOperations (Lude.Maybe ChangeTypeEnum)
moSource = Lens.lens (source :: MergeOperations -> Lude.Maybe ChangeTypeEnum) (\s a -> s {source = a} :: MergeOperations)
{-# DEPRECATED moSource "Use generic-lens or generic-optics with 'source' instead." #-}

instance Lude.FromJSON MergeOperations where
  parseJSON =
    Lude.withObject
      "MergeOperations"
      ( \x ->
          MergeOperations'
            Lude.<$> (x Lude..:? "destination") Lude.<*> (x Lude..:? "source")
      )
