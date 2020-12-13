{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ErrorInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ErrorInfo
  ( ErrorInfo (..),

    -- * Smart constructor
    mkErrorInfo,

    -- * Lenses
    eiCode,
    eiMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Error information.
--
-- /See:/ 'mkErrorInfo' smart constructor.
data ErrorInfo = ErrorInfo'
  { -- | The error code.
    code :: Lude.Maybe Lude.Text,
    -- | The error message.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ErrorInfo' with the minimum fields required to make a request.
--
-- * 'code' - The error code.
-- * 'message' - The error message.
mkErrorInfo ::
  ErrorInfo
mkErrorInfo =
  ErrorInfo' {code = Lude.Nothing, message = Lude.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiCode :: Lens.Lens' ErrorInfo (Lude.Maybe Lude.Text)
eiCode = Lens.lens (code :: ErrorInfo -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: ErrorInfo)
{-# DEPRECATED eiCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiMessage :: Lens.Lens' ErrorInfo (Lude.Maybe Lude.Text)
eiMessage = Lens.lens (message :: ErrorInfo -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ErrorInfo)
{-# DEPRECATED eiMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ErrorInfo where
  parseJSON =
    Lude.withObject
      "ErrorInfo"
      ( \x ->
          ErrorInfo'
            Lude.<$> (x Lude..:? "code") Lude.<*> (x Lude..:? "message")
      )
