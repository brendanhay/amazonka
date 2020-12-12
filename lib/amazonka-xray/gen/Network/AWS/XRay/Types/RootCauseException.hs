{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.RootCauseException
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.RootCauseException
  ( RootCauseException (..),

    -- * Smart constructor
    mkRootCauseException,

    -- * Lenses
    rceName,
    rceMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The exception associated with a root cause.
--
-- /See:/ 'mkRootCauseException' smart constructor.
data RootCauseException = RootCauseException'
  { name ::
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

-- | Creates a value of 'RootCauseException' with the minimum fields required to make a request.
--
-- * 'message' - The message of the exception.
-- * 'name' - The name of the exception.
mkRootCauseException ::
  RootCauseException
mkRootCauseException =
  RootCauseException' {name = Lude.Nothing, message = Lude.Nothing}

-- | The name of the exception.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceName :: Lens.Lens' RootCauseException (Lude.Maybe Lude.Text)
rceName = Lens.lens (name :: RootCauseException -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: RootCauseException)
{-# DEPRECATED rceName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The message of the exception.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceMessage :: Lens.Lens' RootCauseException (Lude.Maybe Lude.Text)
rceMessage = Lens.lens (message :: RootCauseException -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: RootCauseException)
{-# DEPRECATED rceMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON RootCauseException where
  parseJSON =
    Lude.withObject
      "RootCauseException"
      ( \x ->
          RootCauseException'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "Message")
      )
