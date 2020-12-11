-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.DeleteFleetError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.DeleteFleetError
  ( DeleteFleetError (..),

    -- * Smart constructor
    mkDeleteFleetError,

    -- * Lenses
    dfeCode,
    dfeMessage,
  )
where

import Network.AWS.EC2.Types.DeleteFleetErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an EC2 Fleet error.
--
-- /See:/ 'mkDeleteFleetError' smart constructor.
data DeleteFleetError = DeleteFleetError'
  { code ::
      Lude.Maybe DeleteFleetErrorCode,
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

-- | Creates a value of 'DeleteFleetError' with the minimum fields required to make a request.
--
-- * 'code' - The error code.
-- * 'message' - The description for the error code.
mkDeleteFleetError ::
  DeleteFleetError
mkDeleteFleetError =
  DeleteFleetError' {code = Lude.Nothing, message = Lude.Nothing}

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeCode :: Lens.Lens' DeleteFleetError (Lude.Maybe DeleteFleetErrorCode)
dfeCode = Lens.lens (code :: DeleteFleetError -> Lude.Maybe DeleteFleetErrorCode) (\s a -> s {code = a} :: DeleteFleetError)
{-# DEPRECATED dfeCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The description for the error code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfeMessage :: Lens.Lens' DeleteFleetError (Lude.Maybe Lude.Text)
dfeMessage = Lens.lens (message :: DeleteFleetError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: DeleteFleetError)
{-# DEPRECATED dfeMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML DeleteFleetError where
  parseXML x =
    DeleteFleetError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
