{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnError
  ( ColumnError (..),

    -- * Smart constructor
    mkColumnError,

    -- * Lenses
    ceError,
    ceColumnName,
  )
where

import Network.AWS.Glue.Types.ErrorDetail
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Encapsulates a column name that failed and the reason for failure.
--
-- /See:/ 'mkColumnError' smart constructor.
data ColumnError = ColumnError'
  { -- | An error message with the reason for the failure of an operation.
    error :: Lude.Maybe ErrorDetail,
    -- | The name of the column that failed.
    columnName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ColumnError' with the minimum fields required to make a request.
--
-- * 'error' - An error message with the reason for the failure of an operation.
-- * 'columnName' - The name of the column that failed.
mkColumnError ::
  ColumnError
mkColumnError =
  ColumnError' {error = Lude.Nothing, columnName = Lude.Nothing}

-- | An error message with the reason for the failure of an operation.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceError :: Lens.Lens' ColumnError (Lude.Maybe ErrorDetail)
ceError = Lens.lens (error :: ColumnError -> Lude.Maybe ErrorDetail) (\s a -> s {error = a} :: ColumnError)
{-# DEPRECATED ceError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | The name of the column that failed.
--
-- /Note:/ Consider using 'columnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ceColumnName :: Lens.Lens' ColumnError (Lude.Maybe Lude.Text)
ceColumnName = Lens.lens (columnName :: ColumnError -> Lude.Maybe Lude.Text) (\s a -> s {columnName = a} :: ColumnError)
{-# DEPRECATED ceColumnName "Use generic-lens or generic-optics with 'columnName' instead." #-}

instance Lude.FromJSON ColumnError where
  parseJSON =
    Lude.withObject
      "ColumnError"
      ( \x ->
          ColumnError'
            Lude.<$> (x Lude..:? "Error") Lude.<*> (x Lude..:? "ColumnName")
      )
