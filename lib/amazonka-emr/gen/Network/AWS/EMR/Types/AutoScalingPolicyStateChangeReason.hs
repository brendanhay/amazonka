-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
  ( AutoScalingPolicyStateChangeReason (..),

    -- * Smart constructor
    mkAutoScalingPolicyStateChangeReason,

    -- * Lenses
    aspscrCode,
    aspscrMessage,
  )
where

import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The reason for an 'AutoScalingPolicyStatus' change.
--
-- /See:/ 'mkAutoScalingPolicyStateChangeReason' smart constructor.
data AutoScalingPolicyStateChangeReason = AutoScalingPolicyStateChangeReason'
  { code ::
      Lude.Maybe
        AutoScalingPolicyStateChangeReasonCode,
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

-- | Creates a value of 'AutoScalingPolicyStateChangeReason' with the minimum fields required to make a request.
--
-- * 'code' - The code indicating the reason for the change in status.@USER_REQUEST@ indicates that the scaling policy status was changed by a user. @PROVISION_FAILURE@ indicates that the status change was because the policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
-- * 'message' - A friendly, more verbose message that accompanies an automatic scaling policy state change.
mkAutoScalingPolicyStateChangeReason ::
  AutoScalingPolicyStateChangeReason
mkAutoScalingPolicyStateChangeReason =
  AutoScalingPolicyStateChangeReason'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The code indicating the reason for the change in status.@USER_REQUEST@ indicates that the scaling policy status was changed by a user. @PROVISION_FAILURE@ indicates that the status change was because the policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspscrCode :: Lens.Lens' AutoScalingPolicyStateChangeReason (Lude.Maybe AutoScalingPolicyStateChangeReasonCode)
aspscrCode = Lens.lens (code :: AutoScalingPolicyStateChangeReason -> Lude.Maybe AutoScalingPolicyStateChangeReasonCode) (\s a -> s {code = a} :: AutoScalingPolicyStateChangeReason)
{-# DEPRECATED aspscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | A friendly, more verbose message that accompanies an automatic scaling policy state change.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aspscrMessage :: Lens.Lens' AutoScalingPolicyStateChangeReason (Lude.Maybe Lude.Text)
aspscrMessage = Lens.lens (message :: AutoScalingPolicyStateChangeReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: AutoScalingPolicyStateChangeReason)
{-# DEPRECATED aspscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON AutoScalingPolicyStateChangeReason where
  parseJSON =
    Lude.withObject
      "AutoScalingPolicyStateChangeReason"
      ( \x ->
          AutoScalingPolicyStateChangeReason'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )
