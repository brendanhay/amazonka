{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EnvironmentResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.EnvironmentResponse
  ( EnvironmentResponse (..),

    -- * Smart constructor
    mkEnvironmentResponse,

    -- * Lenses
    efVariables,
    efError,
  )
where

import Network.AWS.Lambda.Types.EnvironmentError
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The results of an operation to update or read environment variables. If the operation is successful, the response contains the environment variables. If it failed, the response contains details about the error.
--
-- /See:/ 'mkEnvironmentResponse' smart constructor.
data EnvironmentResponse = EnvironmentResponse'
  { -- | Environment variable key-value pairs.
    variables :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text)),
    -- | Error messages for environment variables that couldn't be applied.
    error :: Lude.Maybe EnvironmentError
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnvironmentResponse' with the minimum fields required to make a request.
--
-- * 'variables' - Environment variable key-value pairs.
-- * 'error' - Error messages for environment variables that couldn't be applied.
mkEnvironmentResponse ::
  EnvironmentResponse
mkEnvironmentResponse =
  EnvironmentResponse'
    { variables = Lude.Nothing,
      error = Lude.Nothing
    }

-- | Environment variable key-value pairs.
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efVariables :: Lens.Lens' EnvironmentResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text)))
efVariables = Lens.lens (variables :: EnvironmentResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text))) (\s a -> s {variables = a} :: EnvironmentResponse)
{-# DEPRECATED efVariables "Use generic-lens or generic-optics with 'variables' instead." #-}

-- | Error messages for environment variables that couldn't be applied.
--
-- /Note:/ Consider using 'error' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
efError :: Lens.Lens' EnvironmentResponse (Lude.Maybe EnvironmentError)
efError = Lens.lens (error :: EnvironmentResponse -> Lude.Maybe EnvironmentError) (\s a -> s {error = a} :: EnvironmentResponse)
{-# DEPRECATED efError "Use generic-lens or generic-optics with 'error' instead." #-}

instance Lude.FromJSON EnvironmentResponse where
  parseJSON =
    Lude.withObject
      "EnvironmentResponse"
      ( \x ->
          EnvironmentResponse'
            Lude.<$> (x Lude..:? "Variables" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Error")
      )
