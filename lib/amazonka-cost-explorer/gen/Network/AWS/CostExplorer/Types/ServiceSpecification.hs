{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ServiceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ServiceSpecification
  ( ServiceSpecification (..),

    -- * Smart constructor
    mkServiceSpecification,

    -- * Lenses
    ssEC2Specification,
  )
where

import Network.AWS.CostExplorer.Types.EC2Specification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Hardware specifications for the service that you want recommendations for.
--
-- /See:/ 'mkServiceSpecification' smart constructor.
newtype ServiceSpecification = ServiceSpecification'
  { ec2Specification ::
      Lude.Maybe EC2Specification
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServiceSpecification' with the minimum fields required to make a request.
--
-- * 'ec2Specification' - The Amazon EC2 hardware specifications that you want AWS to provide recommendations for.
mkServiceSpecification ::
  ServiceSpecification
mkServiceSpecification =
  ServiceSpecification' {ec2Specification = Lude.Nothing}

-- | The Amazon EC2 hardware specifications that you want AWS to provide recommendations for.
--
-- /Note:/ Consider using 'ec2Specification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssEC2Specification :: Lens.Lens' ServiceSpecification (Lude.Maybe EC2Specification)
ssEC2Specification = Lens.lens (ec2Specification :: ServiceSpecification -> Lude.Maybe EC2Specification) (\s a -> s {ec2Specification = a} :: ServiceSpecification)
{-# DEPRECATED ssEC2Specification "Use generic-lens or generic-optics with 'ec2Specification' instead." #-}

instance Lude.FromJSON ServiceSpecification where
  parseJSON =
    Lude.withObject
      "ServiceSpecification"
      ( \x ->
          ServiceSpecification' Lude.<$> (x Lude..:? "EC2Specification")
      )

instance Lude.ToJSON ServiceSpecification where
  toJSON ServiceSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [("EC2Specification" Lude..=) Lude.<$> ec2Specification]
      )
