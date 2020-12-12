{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ResourceUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ResourceUtilization
  ( ResourceUtilization (..),

    -- * Smart constructor
    mkResourceUtilization,

    -- * Lenses
    ruEC2ResourceUtilization,
  )
where

import Network.AWS.CostExplorer.Types.EC2ResourceUtilization
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Resource utilization of current resource.
--
-- /See:/ 'mkResourceUtilization' smart constructor.
newtype ResourceUtilization = ResourceUtilization'
  { ec2ResourceUtilization ::
      Lude.Maybe EC2ResourceUtilization
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceUtilization' with the minimum fields required to make a request.
--
-- * 'ec2ResourceUtilization' - Utilization of current Amazon EC2 instance.
mkResourceUtilization ::
  ResourceUtilization
mkResourceUtilization =
  ResourceUtilization' {ec2ResourceUtilization = Lude.Nothing}

-- | Utilization of current Amazon EC2 instance.
--
-- /Note:/ Consider using 'ec2ResourceUtilization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ruEC2ResourceUtilization :: Lens.Lens' ResourceUtilization (Lude.Maybe EC2ResourceUtilization)
ruEC2ResourceUtilization = Lens.lens (ec2ResourceUtilization :: ResourceUtilization -> Lude.Maybe EC2ResourceUtilization) (\s a -> s {ec2ResourceUtilization = a} :: ResourceUtilization)
{-# DEPRECATED ruEC2ResourceUtilization "Use generic-lens or generic-optics with 'ec2ResourceUtilization' instead." #-}

instance Lude.FromJSON ResourceUtilization where
  parseJSON =
    Lude.withObject
      "ResourceUtilization"
      ( \x ->
          ResourceUtilization'
            Lude.<$> (x Lude..:? "EC2ResourceUtilization")
      )
