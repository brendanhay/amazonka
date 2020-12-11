-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.BatchDeleteImportDataError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.BatchDeleteImportDataError
  ( BatchDeleteImportDataError (..),

    -- * Smart constructor
    mkBatchDeleteImportDataError,

    -- * Lenses
    bdideImportTaskId,
    bdideErrorCode,
    bdideErrorDescription,
  )
where

import Network.AWS.Discovery.Types.BatchDeleteImportDataErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Error messages returned for each import task that you deleted as a response for this command.
--
-- /See:/ 'mkBatchDeleteImportDataError' smart constructor.
data BatchDeleteImportDataError = BatchDeleteImportDataError'
  { importTaskId ::
      Lude.Maybe Lude.Text,
    errorCode ::
      Lude.Maybe
        BatchDeleteImportDataErrorCode,
    errorDescription ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteImportDataError' with the minimum fields required to make a request.
--
-- * 'errorCode' - The type of error that occurred for a specific import task.
-- * 'errorDescription' - The description of the error that occurred for a specific import task.
-- * 'importTaskId' - The unique import ID associated with the error that occurred.
mkBatchDeleteImportDataError ::
  BatchDeleteImportDataError
mkBatchDeleteImportDataError =
  BatchDeleteImportDataError'
    { importTaskId = Lude.Nothing,
      errorCode = Lude.Nothing,
      errorDescription = Lude.Nothing
    }

-- | The unique import ID associated with the error that occurred.
--
-- /Note:/ Consider using 'importTaskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdideImportTaskId :: Lens.Lens' BatchDeleteImportDataError (Lude.Maybe Lude.Text)
bdideImportTaskId = Lens.lens (importTaskId :: BatchDeleteImportDataError -> Lude.Maybe Lude.Text) (\s a -> s {importTaskId = a} :: BatchDeleteImportDataError)
{-# DEPRECATED bdideImportTaskId "Use generic-lens or generic-optics with 'importTaskId' instead." #-}

-- | The type of error that occurred for a specific import task.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdideErrorCode :: Lens.Lens' BatchDeleteImportDataError (Lude.Maybe BatchDeleteImportDataErrorCode)
bdideErrorCode = Lens.lens (errorCode :: BatchDeleteImportDataError -> Lude.Maybe BatchDeleteImportDataErrorCode) (\s a -> s {errorCode = a} :: BatchDeleteImportDataError)
{-# DEPRECATED bdideErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The description of the error that occurred for a specific import task.
--
-- /Note:/ Consider using 'errorDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdideErrorDescription :: Lens.Lens' BatchDeleteImportDataError (Lude.Maybe Lude.Text)
bdideErrorDescription = Lens.lens (errorDescription :: BatchDeleteImportDataError -> Lude.Maybe Lude.Text) (\s a -> s {errorDescription = a} :: BatchDeleteImportDataError)
{-# DEPRECATED bdideErrorDescription "Use generic-lens or generic-optics with 'errorDescription' instead." #-}

instance Lude.FromJSON BatchDeleteImportDataError where
  parseJSON =
    Lude.withObject
      "BatchDeleteImportDataError"
      ( \x ->
          BatchDeleteImportDataError'
            Lude.<$> (x Lude..:? "importTaskId")
            Lude.<*> (x Lude..:? "errorCode")
            Lude.<*> (x Lude..:? "errorDescription")
      )
