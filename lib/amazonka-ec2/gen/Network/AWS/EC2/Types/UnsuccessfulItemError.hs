{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulItemError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulItemError
  ( UnsuccessfulItemError (..),

    -- * Smart constructor
    mkUnsuccessfulItemError,

    -- * Lenses
    uieCode,
    uieMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the error that occurred. For more information about errors, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/errors-overview.html Error Codes> .
--
-- /See:/ 'mkUnsuccessfulItemError' smart constructor.
data UnsuccessfulItemError = UnsuccessfulItemError'
  { -- | The error code.
    code :: Lude.Maybe Lude.Text,
    -- | The error message accompanying the error code.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnsuccessfulItemError' with the minimum fields required to make a request.
--
-- * 'code' - The error code.
-- * 'message' - The error message accompanying the error code.
mkUnsuccessfulItemError ::
  UnsuccessfulItemError
mkUnsuccessfulItemError =
  UnsuccessfulItemError'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uieCode :: Lens.Lens' UnsuccessfulItemError (Lude.Maybe Lude.Text)
uieCode = Lens.lens (code :: UnsuccessfulItemError -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: UnsuccessfulItemError)
{-# DEPRECATED uieCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The error message accompanying the error code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uieMessage :: Lens.Lens' UnsuccessfulItemError (Lude.Maybe Lude.Text)
uieMessage = Lens.lens (message :: UnsuccessfulItemError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: UnsuccessfulItemError)
{-# DEPRECATED uieMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML UnsuccessfulItemError where
  parseXML x =
    UnsuccessfulItemError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
