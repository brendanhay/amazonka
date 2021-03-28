{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BackfillError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.BackfillError
  ( BackfillError (..)
  -- * Smart constructor
  , mkBackfillError
  -- * Lenses
  , beCode
  , bePartitions
  ) where

import qualified Network.AWS.Glue.Types.BackfillErrorCode as Types
import qualified Network.AWS.Glue.Types.PartitionValueList as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A list of errors that can occur when registering partition indexes for an existing table.
--
-- These errors give the details about why an index registration failed and provide a limited number of partitions in the response, so that you can fix the partitions at fault and try registering the index again. The most common set of errors that can occur are categorized as follows:
--
--     * EncryptedPartitionError: The partitions are encrypted.
--
--
--     * InvalidPartitionTypeDataError: The partition value doesn't match the data type for that partition column.
--
--
--     * MissingPartitionValueError: The partitions are encrypted.
--
--
--     * UnsupportedPartitionCharacterError: Characters inside the partition value are not supported. For example: U+0000 , U+0001, U+0002.
--
--
--     * InternalError: Any error which does not belong to other error codes.
--
--
--
-- /See:/ 'mkBackfillError' smart constructor.
data BackfillError = BackfillError'
  { code :: Core.Maybe Types.BackfillErrorCode
    -- ^ The error code for an error that occurred when registering partition indexes for an existing table.
  , partitions :: Core.Maybe [Types.PartitionValueList]
    -- ^ A list of a limited number of partitions in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BackfillError' value with any optional fields omitted.
mkBackfillError
    :: BackfillError
mkBackfillError
  = BackfillError'{code = Core.Nothing, partitions = Core.Nothing}

-- | The error code for an error that occurred when registering partition indexes for an existing table.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
beCode :: Lens.Lens' BackfillError (Core.Maybe Types.BackfillErrorCode)
beCode = Lens.field @"code"
{-# INLINEABLE beCode #-}
{-# DEPRECATED code "Use generic-lens or generic-optics with 'code' instead"  #-}

-- | A list of a limited number of partitions in the response.
--
-- /Note:/ Consider using 'partitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bePartitions :: Lens.Lens' BackfillError (Core.Maybe [Types.PartitionValueList])
bePartitions = Lens.field @"partitions"
{-# INLINEABLE bePartitions #-}
{-# DEPRECATED partitions "Use generic-lens or generic-optics with 'partitions' instead"  #-}

instance Core.FromJSON BackfillError where
        parseJSON
          = Core.withObject "BackfillError" Core.$
              \ x ->
                BackfillError' Core.<$>
                  (x Core..:? "Code") Core.<*> x Core..:? "Partitions"
