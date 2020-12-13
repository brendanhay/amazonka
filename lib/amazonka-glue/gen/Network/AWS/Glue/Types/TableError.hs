{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.TableError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.TableError
  ( TableError (..),

    -- * Smart constructor
    mkTableError,

    -- * Lenses
    teTableName,
    teErrorDetail,
  )
where

import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An error record for table operations.
--
-- /See:/ 'mkTableError' smart constructor.
data TableError = TableError'
  { -- | The name of the table. For Hive compatibility, this must be entirely lowercase.
    tableName :: Lude.Maybe Lude.Text,
    -- | The details about the error.
    errorDetail :: Lude.Maybe ErrorDetail
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TableError' with the minimum fields required to make a request.
--
-- * 'tableName' - The name of the table. For Hive compatibility, this must be entirely lowercase.
-- * 'errorDetail' - The details about the error.
mkTableError ::
  TableError
mkTableError =
  TableError' {tableName = Lude.Nothing, errorDetail = Lude.Nothing}

-- | The name of the table. For Hive compatibility, this must be entirely lowercase.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teTableName :: Lens.Lens' TableError (Lude.Maybe Lude.Text)
teTableName = Lens.lens (tableName :: TableError -> Lude.Maybe Lude.Text) (\s a -> s {tableName = a} :: TableError)
{-# DEPRECATED teTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

-- | The details about the error.
--
-- /Note:/ Consider using 'errorDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
teErrorDetail :: Lens.Lens' TableError (Lude.Maybe ErrorDetail)
teErrorDetail = Lens.lens (errorDetail :: TableError -> Lude.Maybe ErrorDetail) (\s a -> s {errorDetail = a} :: TableError)
{-# DEPRECATED teErrorDetail "Use generic-lens or generic-optics with 'errorDetail' instead." #-}

instance Lude.FromJSON TableError where
  parseJSON =
    Lude.withObject
      "TableError"
      ( \x ->
          TableError'
            Lude.<$> (x Lude..:? "TableName") Lude.<*> (x Lude..:? "ErrorDetail")
      )
