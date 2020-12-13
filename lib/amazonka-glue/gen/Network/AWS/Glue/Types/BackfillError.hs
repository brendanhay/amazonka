{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BackfillError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BackfillError
  ( BackfillError (..),

    -- * Smart constructor
    mkBackfillError,

    -- * Lenses
    bePartitions,
    beCode,
  )
where

import Network.AWS.Glue.Types.BackfillErrorCode
import Network.AWS.Glue.Types.PartitionValueList
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { -- | A list of a limited number of partitions in the response.
    partitions :: Lude.Maybe [PartitionValueList],
    -- | The error code for an error that occurred when registering partition indexes for an existing table.
    code :: Lude.Maybe BackfillErrorCode
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BackfillError' with the minimum fields required to make a request.
--
-- * 'partitions' - A list of a limited number of partitions in the response.
-- * 'code' - The error code for an error that occurred when registering partition indexes for an existing table.
mkBackfillError ::
  BackfillError
mkBackfillError =
  BackfillError' {partitions = Lude.Nothing, code = Lude.Nothing}

-- | A list of a limited number of partitions in the response.
--
-- /Note:/ Consider using 'partitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bePartitions :: Lens.Lens' BackfillError (Lude.Maybe [PartitionValueList])
bePartitions = Lens.lens (partitions :: BackfillError -> Lude.Maybe [PartitionValueList]) (\s a -> s {partitions = a} :: BackfillError)
{-# DEPRECATED bePartitions "Use generic-lens or generic-optics with 'partitions' instead." #-}

-- | The error code for an error that occurred when registering partition indexes for an existing table.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
beCode :: Lens.Lens' BackfillError (Lude.Maybe BackfillErrorCode)
beCode = Lens.lens (code :: BackfillError -> Lude.Maybe BackfillErrorCode) (\s a -> s {code = a} :: BackfillError)
{-# DEPRECATED beCode "Use generic-lens or generic-optics with 'code' instead." #-}

instance Lude.FromJSON BackfillError where
  parseJSON =
    Lude.withObject
      "BackfillError"
      ( \x ->
          BackfillError'
            Lude.<$> (x Lude..:? "Partitions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Code")
      )
