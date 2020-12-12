{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LastError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LastError
  ( LastError (..),

    -- * Smart constructor
    mkLastError,

    -- * Lenses
    leCode,
    leMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The last error that occurred for a VPC endpoint.
--
-- /See:/ 'mkLastError' smart constructor.
data LastError = LastError'
  { code :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'LastError' with the minimum fields required to make a request.
--
-- * 'code' - The error code for the VPC endpoint error.
-- * 'message' - The error message for the VPC endpoint error.
mkLastError ::
  LastError
mkLastError =
  LastError' {code = Lude.Nothing, message = Lude.Nothing}

-- | The error code for the VPC endpoint error.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leCode :: Lens.Lens' LastError (Lude.Maybe Lude.Text)
leCode = Lens.lens (code :: LastError -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: LastError)
{-# DEPRECATED leCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message for the VPC endpoint error.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
leMessage :: Lens.Lens' LastError (Lude.Maybe Lude.Text)
leMessage = Lens.lens (message :: LastError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: LastError)
{-# DEPRECATED leMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML LastError where
  parseXML x =
    LastError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
