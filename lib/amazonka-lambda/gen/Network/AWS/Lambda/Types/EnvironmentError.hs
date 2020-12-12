{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EnvironmentError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.EnvironmentError
  ( EnvironmentError (..),

    -- * Smart constructor
    mkEnvironmentError,

    -- * Lenses
    eeErrorCode,
    eeMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Error messages for environment variables that couldn't be applied.
--
-- /See:/ 'mkEnvironmentError' smart constructor.
data EnvironmentError = EnvironmentError'
  { errorCode ::
      Lude.Maybe Lude.Text,
    message :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentError' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code.
-- * 'message' - The error message.
mkEnvironmentError ::
  EnvironmentError
mkEnvironmentError =
  EnvironmentError'
    { errorCode = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeErrorCode :: Lens.Lens' EnvironmentError (Lude.Maybe Lude.Text)
eeErrorCode = Lens.lens (errorCode :: EnvironmentError -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: EnvironmentError)
{-# DEPRECATED eeErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeMessage :: Lens.Lens' EnvironmentError (Lude.Maybe (Lude.Sensitive Lude.Text))
eeMessage = Lens.lens (message :: EnvironmentError -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {message = a} :: EnvironmentError)
{-# DEPRECATED eeMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON EnvironmentError where
  parseJSON =
    Lude.withObject
      "EnvironmentError"
      ( \x ->
          EnvironmentError'
            Lude.<$> (x Lude..:? "ErrorCode") Lude.<*> (x Lude..:? "Message")
      )
