-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Environment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.Environment
  ( Environment (..),

    -- * Smart constructor
    mkEnvironment,

    -- * Lenses
    eVariables,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A function's environment variable settings.
--
-- /See:/ 'mkEnvironment' smart constructor.
newtype Environment = Environment'
  { variables ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text))
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Environment' with the minimum fields required to make a request.
--
-- * 'variables' - Environment variable key-value pairs.
mkEnvironment ::
  Environment
mkEnvironment = Environment' {variables = Lude.Nothing}

-- | Environment variable key-value pairs.
--
-- /Note:/ Consider using 'variables' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eVariables :: Lens.Lens' Environment (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text)))
eVariables = Lens.lens (variables :: Environment -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Sensitive Lude.Text))) (\s a -> s {variables = a} :: Environment)
{-# DEPRECATED eVariables "Use generic-lens or generic-optics with 'variables' instead." #-}

instance Lude.ToJSON Environment where
  toJSON Environment' {..} =
    Lude.object
      (Lude.catMaybes [("Variables" Lude..=) Lude.<$> variables])
