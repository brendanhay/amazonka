-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableVersionError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableVersionError
  ( TableVersionError (..),

    -- * Smart constructor
    mkTableVersionError,

    -- * Lenses
    tveVersionId,
    tveTableName,
    tveErrorDetail,
  )
where

import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An error record for table-version operations.
--
-- /See:/ 'mkTableVersionError' smart constructor.
data TableVersionError = TableVersionError'
  { versionId ::
      Lude.Maybe Lude.Text,
    tableName :: Lude.Maybe Lude.Text,
    errorDetail :: Lude.Maybe ErrorDetail
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableVersionError' with the minimum fields required to make a request.
--
-- * 'errorDetail' - The details about the error.
-- * 'tableName' - The name of the table in question.
-- * 'versionId' - The ID value of the version in question. A @VersionID@ is a string representation of an integer. Each version is incremented by 1.
mkTableVersionError ::
  TableVersionError
mkTableVersionError =
  TableVersionError'
    { versionId = Lude.Nothing,
      tableName = Lude.Nothing,
      errorDetail = Lude.Nothing
    }

-- | The ID value of the version in question. A @VersionID@ is a string representation of an integer. Each version is incremented by 1.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tveVersionId :: Lens.Lens' TableVersionError (Lude.Maybe Lude.Text)
tveVersionId = Lens.lens (versionId :: TableVersionError -> Lude.Maybe Lude.Text) (\s a -> s {versionId = a} :: TableVersionError)
{-# DEPRECATED tveVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The name of the table in question.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tveTableName :: Lens.Lens' TableVersionError (Lude.Maybe Lude.Text)
tveTableName = Lens.lens (tableName :: TableVersionError -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: TableVersionError)
{-# DEPRECATED tveTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The details about the error.
--
-- /Note:/ Consider using 'errorDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tveErrorDetail :: Lens.Lens' TableVersionError (Lude.Maybe ErrorDetail)
tveErrorDetail = Lens.lens (errorDetail :: TableVersionError -> Lude.Maybe ErrorDetail) (\s a -> s {errorDetail = a} :: TableVersionError)
{-# DEPRECATED tveErrorDetail "Use generic-lens or generic-optics with 'errorDetail' instead." #-}

instance Lude.FromJSON TableVersionError where
  parseJSON =
    Lude.withObject
      "TableVersionError"
      ( \x ->
          TableVersionError'
            Lude.<$> (x Lude..:? "VersionId")
            Lude.<*> (x Lude..:? "TableName")
            Lude.<*> (x Lude..:? "ErrorDetail")
      )
