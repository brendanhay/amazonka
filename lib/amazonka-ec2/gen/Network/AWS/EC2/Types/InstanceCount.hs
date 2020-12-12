{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceCount
  ( InstanceCount (..),

    -- * Smart constructor
    mkInstanceCount,

    -- * Lenses
    icState,
    icInstanceCount,
  )
where

import Network.AWS.EC2.Types.ListingState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a Reserved Instance listing state.
--
-- /See:/ 'mkInstanceCount' smart constructor.
data InstanceCount = InstanceCount'
  { state ::
      Lude.Maybe ListingState,
    instanceCount :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceCount' with the minimum fields required to make a request.
--
-- * 'instanceCount' - The number of listed Reserved Instances in the state specified by the @state@ .
-- * 'state' - The states of the listed Reserved Instances.
mkInstanceCount ::
  InstanceCount
mkInstanceCount =
  InstanceCount'
    { state = Lude.Nothing,
      instanceCount = Lude.Nothing
    }

-- | The states of the listed Reserved Instances.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icState :: Lens.Lens' InstanceCount (Lude.Maybe ListingState)
icState = Lens.lens (state :: InstanceCount -> Lude.Maybe ListingState) (\s a -> s {state = a} :: InstanceCount)
{-# DEPRECATED icState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The number of listed Reserved Instances in the state specified by the @state@ .
--
-- /Note:/ Consider using 'instanceCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icInstanceCount :: Lens.Lens' InstanceCount (Lude.Maybe Lude.Int)
icInstanceCount = Lens.lens (instanceCount :: InstanceCount -> Lude.Maybe Lude.Int) (\s a -> s {instanceCount = a} :: InstanceCount)
{-# DEPRECATED icInstanceCount "Use generic-lens or generic-optics with 'instanceCount' instead." #-}

instance Lude.FromXML InstanceCount where
  parseXML x =
    InstanceCount'
      Lude.<$> (x Lude..@? "state") Lude.<*> (x Lude..@? "instanceCount")
