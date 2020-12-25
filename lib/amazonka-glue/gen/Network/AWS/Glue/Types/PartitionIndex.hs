{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.PartitionIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.PartitionIndex
  ( PartitionIndex (..),

    -- * Smart constructor
    mkPartitionIndex,

    -- * Lenses
    piKeys,
    piIndexName,
  )
where

import qualified Network.AWS.Glue.Types.IndexName as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A structure for a partition index.
--
-- /See:/ 'mkPartitionIndex' smart constructor.
data PartitionIndex = PartitionIndex'
  { -- | The keys for the partition index.
    keys :: Core.NonEmpty Types.NameString,
    -- | The name of the partition index.
    indexName :: Types.IndexName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PartitionIndex' value with any optional fields omitted.
mkPartitionIndex ::
  -- | 'keys'
  Core.NonEmpty Types.NameString ->
  -- | 'indexName'
  Types.IndexName ->
  PartitionIndex
mkPartitionIndex keys indexName = PartitionIndex' {keys, indexName}

-- | The keys for the partition index.
--
-- /Note:/ Consider using 'keys' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piKeys :: Lens.Lens' PartitionIndex (Core.NonEmpty Types.NameString)
piKeys = Lens.field @"keys"
{-# DEPRECATED piKeys "Use generic-lens or generic-optics with 'keys' instead." #-}

-- | The name of the partition index.
--
-- /Note:/ Consider using 'indexName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piIndexName :: Lens.Lens' PartitionIndex Types.IndexName
piIndexName = Lens.field @"indexName"
{-# DEPRECATED piIndexName "Use generic-lens or generic-optics with 'indexName' instead." #-}

instance Core.FromJSON PartitionIndex where
  toJSON PartitionIndex {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Keys" Core..= keys),
            Core.Just ("IndexName" Core..= indexName)
          ]
      )
