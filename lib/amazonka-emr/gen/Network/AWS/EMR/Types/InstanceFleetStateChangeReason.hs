{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetStateChangeReason
  ( InstanceFleetStateChangeReason (..),

    -- * Smart constructor
    mkInstanceFleetStateChangeReason,

    -- * Lenses
    ifscrCode,
    ifscrMessage,
  )
where

import Network.AWS.EMR.Types.InstanceFleetStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides status change reason details for the instance fleet.
--
-- /See:/ 'mkInstanceFleetStateChangeReason' smart constructor.
data InstanceFleetStateChangeReason = InstanceFleetStateChangeReason'
  { code ::
      Lude.Maybe
        InstanceFleetStateChangeReasonCode,
    message ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceFleetStateChangeReason' with the minimum fields required to make a request.
--
-- * 'code' - A code corresponding to the reason the state change occurred.
-- * 'message' - An explanatory message.
mkInstanceFleetStateChangeReason ::
  InstanceFleetStateChangeReason
mkInstanceFleetStateChangeReason =
  InstanceFleetStateChangeReason'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | A code corresponding to the reason the state change occurred.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifscrCode :: Lens.Lens' InstanceFleetStateChangeReason (Lude.Maybe InstanceFleetStateChangeReasonCode)
ifscrCode = Lens.lens (code :: InstanceFleetStateChangeReason -> Lude.Maybe InstanceFleetStateChangeReasonCode) (\s a -> s {code = a} :: InstanceFleetStateChangeReason)
{-# DEPRECATED ifscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | An explanatory message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ifscrMessage :: Lens.Lens' InstanceFleetStateChangeReason (Lude.Maybe Lude.Text)
ifscrMessage = Lens.lens (message :: InstanceFleetStateChangeReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: InstanceFleetStateChangeReason)
{-# DEPRECATED ifscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON InstanceFleetStateChangeReason where
  parseJSON =
    Lude.withObject
      "InstanceFleetStateChangeReason"
      ( \x ->
          InstanceFleetStateChangeReason'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )
