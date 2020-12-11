-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.BundleTaskError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.BundleTaskError
  ( BundleTaskError (..),

    -- * Smart constructor
    mkBundleTaskError,

    -- * Lenses
    bteCode,
    bteMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an error for 'BundleInstance' .
--
-- /See:/ 'mkBundleTaskError' smart constructor.
data BundleTaskError = BundleTaskError'
  { code ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'BundleTaskError' with the minimum fields required to make a request.
--
-- * 'code' - The error code.
-- * 'message' - The error message.
mkBundleTaskError ::
  BundleTaskError
mkBundleTaskError =
  BundleTaskError' {code = Lude.Nothing, message = Lude.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bteCode :: Lens.Lens' BundleTaskError (Lude.Maybe Lude.Text)
bteCode = Lens.lens (code :: BundleTaskError -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: BundleTaskError)
{-# DEPRECATED bteCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bteMessage :: Lens.Lens' BundleTaskError (Lude.Maybe Lude.Text)
bteMessage = Lens.lens (message :: BundleTaskError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: BundleTaskError)
{-# DEPRECATED bteMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML BundleTaskError where
  parseXML x =
    BundleTaskError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
