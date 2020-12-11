-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceStateChangeReason
  ( InstanceStateChangeReason (..),

    -- * Smart constructor
    mkInstanceStateChangeReason,

    -- * Lenses
    iscrCode,
    iscrMessage,
  )
where

import Network.AWS.EMR.Types.InstanceStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of the status change reason for the instance.
--
-- /See:/ 'mkInstanceStateChangeReason' smart constructor.
data InstanceStateChangeReason = InstanceStateChangeReason'
  { code ::
      Lude.Maybe
        InstanceStateChangeReasonCode,
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

-- | Creates a value of 'InstanceStateChangeReason' with the minimum fields required to make a request.
--
-- * 'code' - The programmable code for the state change reason.
-- * 'message' - The status change reason description.
mkInstanceStateChangeReason ::
  InstanceStateChangeReason
mkInstanceStateChangeReason =
  InstanceStateChangeReason'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The programmable code for the state change reason.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrCode :: Lens.Lens' InstanceStateChangeReason (Lude.Maybe InstanceStateChangeReasonCode)
iscrCode = Lens.lens (code :: InstanceStateChangeReason -> Lude.Maybe InstanceStateChangeReasonCode) (\s a -> s {code = a} :: InstanceStateChangeReason)
{-# DEPRECATED iscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The status change reason description.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscrMessage :: Lens.Lens' InstanceStateChangeReason (Lude.Maybe Lude.Text)
iscrMessage = Lens.lens (message :: InstanceStateChangeReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: InstanceStateChangeReason)
{-# DEPRECATED iscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON InstanceStateChangeReason where
  parseJSON =
    Lude.withObject
      "InstanceStateChangeReason"
      ( \x ->
          InstanceStateChangeReason'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )
