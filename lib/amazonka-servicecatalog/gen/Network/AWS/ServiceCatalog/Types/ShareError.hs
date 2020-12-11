-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ShareError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ShareError
  ( ShareError (..),

    -- * Smart constructor
    mkShareError,

    -- * Lenses
    seAccounts,
    seError,
    seMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Errors that occurred during the portfolio share operation.
--
-- /See:/ 'mkShareError' smart constructor.
data ShareError = ShareError'
  { accounts :: Lude.Maybe [Lude.Text],
    error :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShareError' with the minimum fields required to make a request.
--
-- * 'accounts' - List of accounts impacted by the error.
-- * 'error' - Error type that happened when processing the operation.
-- * 'message' - Information about the error.
mkShareError ::
  ShareError
mkShareError =
  ShareError'
    { accounts = Lude.Nothing,
      error = Lude.Nothing,
      message = Lude.Nothing
    }

-- | List of accounts impacted by the error.
--
-- /Note:/ Consider using 'accounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seAccounts :: Lens.Lens' ShareError (Lude.Maybe [Lude.Text])
seAccounts = Lens.lens (accounts :: ShareError -> Lude.Maybe [Lude.Text]) (\s a -> s {accounts = a} :: ShareError)
{-# DEPRECATED seAccounts "Use generic-lens or generic-optics with 'accounts' instead." #-}

-- | Error type that happened when processing the operation.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seError :: Lens.Lens' ShareError (Lude.Maybe Lude.Text)
seError = Lens.lens (error :: ShareError -> Lude.Maybe Lude.Text) (\s a -> s {error = a} :: ShareError)
{-# DEPRECATED seError "Use generic-lens or generic-optics with 'error' instead." #-}

-- | Information about the error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seMessage :: Lens.Lens' ShareError (Lude.Maybe Lude.Text)
seMessage = Lens.lens (message :: ShareError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ShareError)
{-# DEPRECATED seMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ShareError where
  parseJSON =
    Lude.withObject
      "ShareError"
      ( \x ->
          ShareError'
            Lude.<$> (x Lude..:? "Accounts" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Error")
            Lude.<*> (x Lude..:? "Message")
      )
