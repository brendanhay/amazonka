-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationItemError
  ( UnsuccessfulInstanceCreditSpecificationItemError (..),

    -- * Smart constructor
    mkUnsuccessfulInstanceCreditSpecificationItemError,

    -- * Lenses
    uicsieCode,
    uicsieMessage,
  )
where

import Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the error for the burstable performance instance whose credit option for CPU usage was not modified.
--
-- /See:/ 'mkUnsuccessfulInstanceCreditSpecificationItemError' smart constructor.
data UnsuccessfulInstanceCreditSpecificationItemError = UnsuccessfulInstanceCreditSpecificationItemError'
  { code ::
      Lude.Maybe
        UnsuccessfulInstanceCreditSpecificationErrorCode,
    message ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'UnsuccessfulInstanceCreditSpecificationItemError' with the minimum fields required to make a request.
--
-- * 'code' - The error code.
-- * 'message' - The applicable error message.
mkUnsuccessfulInstanceCreditSpecificationItemError ::
  UnsuccessfulInstanceCreditSpecificationItemError
mkUnsuccessfulInstanceCreditSpecificationItemError =
  UnsuccessfulInstanceCreditSpecificationItemError'
    { code =
        Lude.Nothing,
      message = Lude.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicsieCode :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItemError (Lude.Maybe UnsuccessfulInstanceCreditSpecificationErrorCode)
uicsieCode = Lens.lens (code :: UnsuccessfulInstanceCreditSpecificationItemError -> Lude.Maybe UnsuccessfulInstanceCreditSpecificationErrorCode) (\s a -> s {code = a} :: UnsuccessfulInstanceCreditSpecificationItemError)
{-# DEPRECATED uicsieCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The applicable error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uicsieMessage :: Lens.Lens' UnsuccessfulInstanceCreditSpecificationItemError (Lude.Maybe Lude.Text)
uicsieMessage = Lens.lens (message :: UnsuccessfulInstanceCreditSpecificationItemError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: UnsuccessfulInstanceCreditSpecificationItemError)
{-# DEPRECATED uicsieMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance
  Lude.FromXML
    UnsuccessfulInstanceCreditSpecificationItemError
  where
  parseXML x =
    UnsuccessfulInstanceCreditSpecificationItemError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
