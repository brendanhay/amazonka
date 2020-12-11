-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceGroupStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupStateChangeReason
  ( InstanceGroupStateChangeReason (..),

    -- * Smart constructor
    mkInstanceGroupStateChangeReason,

    -- * Lenses
    igscrCode,
    igscrMessage,
  )
where

import Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The status change reason details for the instance group.
--
-- /See:/ 'mkInstanceGroupStateChangeReason' smart constructor.
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason'
  { code ::
      Lude.Maybe
        InstanceGroupStateChangeReasonCode,
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

-- | Creates a value of 'InstanceGroupStateChangeReason' with the minimum fields required to make a request.
--
-- * 'code' - The programmable code for the state change reason.
-- * 'message' - The status change reason description.
mkInstanceGroupStateChangeReason ::
  InstanceGroupStateChangeReason
mkInstanceGroupStateChangeReason =
  InstanceGroupStateChangeReason'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The programmable code for the state change reason.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igscrCode :: Lens.Lens' InstanceGroupStateChangeReason (Lude.Maybe InstanceGroupStateChangeReasonCode)
igscrCode = Lens.lens (code :: InstanceGroupStateChangeReason -> Lude.Maybe InstanceGroupStateChangeReasonCode) (\s a -> s {code = a} :: InstanceGroupStateChangeReason)
{-# DEPRECATED igscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The status change reason description.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
igscrMessage :: Lens.Lens' InstanceGroupStateChangeReason (Lude.Maybe Lude.Text)
igscrMessage = Lens.lens (message :: InstanceGroupStateChangeReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: InstanceGroupStateChangeReason)
{-# DEPRECATED igscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON InstanceGroupStateChangeReason where
  parseJSON =
    Lude.withObject
      "InstanceGroupStateChangeReason"
      ( \x ->
          InstanceGroupStateChangeReason'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )
