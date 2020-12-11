-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.RecordError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.RecordError
  ( RecordError (..),

    -- * Smart constructor
    mkRecordError,

    -- * Lenses
    reCode,
    reDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The error code and description resulting from an operation.
--
-- /See:/ 'mkRecordError' smart constructor.
data RecordError = RecordError'
  { code :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RecordError' with the minimum fields required to make a request.
--
-- * 'code' - The numeric value of the error.
-- * 'description' - The description of the error.
mkRecordError ::
  RecordError
mkRecordError =
  RecordError' {code = Lude.Nothing, description = Lude.Nothing}

-- | The numeric value of the error.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reCode :: Lens.Lens' RecordError (Lude.Maybe Lude.Text)
reCode = Lens.lens (code :: RecordError -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: RecordError)
{-# DEPRECATED reCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The description of the error.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
reDescription :: Lens.Lens' RecordError (Lude.Maybe Lude.Text)
reDescription = Lens.lens (description :: RecordError -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RecordError)
{-# DEPRECATED reDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON RecordError where
  parseJSON =
    Lude.withObject
      "RecordError"
      ( \x ->
          RecordError'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Description")
      )
