{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterStateChangeReason
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterStateChangeReason
  ( ClusterStateChangeReason (..),

    -- * Smart constructor
    mkClusterStateChangeReason,

    -- * Lenses
    cscrCode,
    cscrMessage,
  )
where

import Network.AWS.EMR.Types.ClusterStateChangeReasonCode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The reason that the cluster changed to its current state.
--
-- /See:/ 'mkClusterStateChangeReason' smart constructor.
data ClusterStateChangeReason = ClusterStateChangeReason'
  { -- | The programmatic code for the state change reason.
    code :: Lude.Maybe ClusterStateChangeReasonCode,
    -- | The descriptive message for the state change reason.
    message :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterStateChangeReason' with the minimum fields required to make a request.
--
-- * 'code' - The programmatic code for the state change reason.
-- * 'message' - The descriptive message for the state change reason.
mkClusterStateChangeReason ::
  ClusterStateChangeReason
mkClusterStateChangeReason =
  ClusterStateChangeReason'
    { code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The programmatic code for the state change reason.
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrCode :: Lens.Lens' ClusterStateChangeReason (Lude.Maybe ClusterStateChangeReasonCode)
cscrCode = Lens.lens (code :: ClusterStateChangeReason -> Lude.Maybe ClusterStateChangeReasonCode) (\s a -> s {code = a} :: ClusterStateChangeReason)
{-# DEPRECATED cscrCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | The descriptive message for the state change reason.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cscrMessage :: Lens.Lens' ClusterStateChangeReason (Lude.Maybe Lude.Text)
cscrMessage = Lens.lens (message :: ClusterStateChangeReason -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: ClusterStateChangeReason)
{-# DEPRECATED cscrMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON ClusterStateChangeReason where
  parseJSON =
    Lude.withObject
      "ClusterStateChangeReason"
      ( \x ->
          ClusterStateChangeReason'
            Lude.<$> (x Lude..:? "Code") Lude.<*> (x Lude..:? "Message")
      )
