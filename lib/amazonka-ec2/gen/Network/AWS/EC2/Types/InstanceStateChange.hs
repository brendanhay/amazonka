{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceStateChange
  ( InstanceStateChange (..),

    -- * Smart constructor
    mkInstanceStateChange,

    -- * Lenses
    iscInstanceId,
    iscCurrentState,
    iscPreviousState,
  )
where

import Network.AWS.EC2.Types.InstanceState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance state change.
--
-- /See:/ 'mkInstanceStateChange' smart constructor.
data InstanceStateChange = InstanceStateChange'
  { -- | The ID of the instance.
    instanceId :: Lude.Maybe Lude.Text,
    -- | The current state of the instance.
    currentState :: Lude.Maybe InstanceState,
    -- | The previous state of the instance.
    previousState :: Lude.Maybe InstanceState
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceStateChange' with the minimum fields required to make a request.
--
-- * 'instanceId' - The ID of the instance.
-- * 'currentState' - The current state of the instance.
-- * 'previousState' - The previous state of the instance.
mkInstanceStateChange ::
  InstanceStateChange
mkInstanceStateChange =
  InstanceStateChange'
    { instanceId = Lude.Nothing,
      currentState = Lude.Nothing,
      previousState = Lude.Nothing
    }

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscInstanceId :: Lens.Lens' InstanceStateChange (Lude.Maybe Lude.Text)
iscInstanceId = Lens.lens (instanceId :: InstanceStateChange -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: InstanceStateChange)
{-# DEPRECATED iscInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The current state of the instance.
--
-- /Note:/ Consider using 'currentState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscCurrentState :: Lens.Lens' InstanceStateChange (Lude.Maybe InstanceState)
iscCurrentState = Lens.lens (currentState :: InstanceStateChange -> Lude.Maybe InstanceState) (\s a -> s {currentState = a} :: InstanceStateChange)
{-# DEPRECATED iscCurrentState "Use generic-lens or generic-optics with 'currentState' instead." #-}

-- | The previous state of the instance.
--
-- /Note:/ Consider using 'previousState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iscPreviousState :: Lens.Lens' InstanceStateChange (Lude.Maybe InstanceState)
iscPreviousState = Lens.lens (previousState :: InstanceStateChange -> Lude.Maybe InstanceState) (\s a -> s {previousState = a} :: InstanceStateChange)
{-# DEPRECATED iscPreviousState "Use generic-lens or generic-optics with 'previousState' instead." #-}

instance Lude.FromXML InstanceStateChange where
  parseXML x =
    InstanceStateChange'
      Lude.<$> (x Lude..@? "instanceId")
      Lude.<*> (x Lude..@? "currentState")
      Lude.<*> (x Lude..@? "previousState")
