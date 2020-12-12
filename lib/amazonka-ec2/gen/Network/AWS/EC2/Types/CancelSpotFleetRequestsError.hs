{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CancelSpotFleetRequestsError
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotFleetRequestsError
  ( CancelSpotFleetRequestsError (..),

    -- * Smart constructor
    mkCancelSpotFleetRequestsError,

    -- * Lenses
    csfreCode,
    csfreMessage,
  )
where

import Network.AWS.EC2.Types.CancelBatchErrorCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Spot Fleet error.
--
-- /See:/ 'mkCancelSpotFleetRequestsError' smart constructor.
data CancelSpotFleetRequestsError = CancelSpotFleetRequestsError'
  { code ::
      Lude.Maybe CancelBatchErrorCode,
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

-- | Creates a value of 'CancelSpotFleetRequestsError' with the minimum fields required to make a request.
--
-- * 'code' - The error code.
-- * 'message' - The description for the error code.
mkCancelSpotFleetRequestsError ::
  CancelSpotFleetRequestsError
mkCancelSpotFleetRequestsError =
  CancelSpotFleetRequestsError'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The error code.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfreCode :: Lens.Lens' CancelSpotFleetRequestsError (Lude.Maybe CancelBatchErrorCode)
csfreCode = Lens.lens (code :: CancelSpotFleetRequestsError -> Lude.Maybe CancelBatchErrorCode) (\s a -> s {code = a} :: CancelSpotFleetRequestsError)
{-# DEPRECATED csfreCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The description for the error code.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csfreMessage :: Lens.Lens' CancelSpotFleetRequestsError (Lude.Maybe Lude.Text)
csfreMessage = Lens.lens (message :: CancelSpotFleetRequestsError -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: CancelSpotFleetRequestsError)
{-# DEPRECATED csfreMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromXML CancelSpotFleetRequestsError where
  parseXML x =
    CancelSpotFleetRequestsError'
      Lude.<$> (x Lude..@? "code") Lude.<*> (x Lude..@? "message")
