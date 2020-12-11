-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ResourceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ResourceDetails
  ( ResourceDetails (..),

    -- * Smart constructor
    mkResourceDetails,

    -- * Lenses
    rdEC2ResourceDetails,
  )
where

import Network.AWS.CostExplorer.Types.EC2ResourceDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details on the resource.
--
-- /See:/ 'mkResourceDetails' smart constructor.
newtype ResourceDetails = ResourceDetails'
  { ec2ResourceDetails ::
      Lude.Maybe EC2ResourceDetails
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourceDetails' with the minimum fields required to make a request.
--
-- * 'ec2ResourceDetails' - Details on the Amazon EC2 resource.
mkResourceDetails ::
  ResourceDetails
mkResourceDetails =
  ResourceDetails' {ec2ResourceDetails = Lude.Nothing}

-- | Details on the Amazon EC2 resource.
--
-- /Note:/ Consider using 'ec2ResourceDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdEC2ResourceDetails :: Lens.Lens' ResourceDetails (Lude.Maybe EC2ResourceDetails)
rdEC2ResourceDetails = Lens.lens (ec2ResourceDetails :: ResourceDetails -> Lude.Maybe EC2ResourceDetails) (\s a -> s {ec2ResourceDetails = a} :: ResourceDetails)
{-# DEPRECATED rdEC2ResourceDetails "Use generic-lens or generic-optics with 'ec2ResourceDetails' instead." #-}

instance Lude.FromJSON ResourceDetails where
  parseJSON =
    Lude.withObject
      "ResourceDetails"
      ( \x ->
          ResourceDetails' Lude.<$> (x Lude..:? "EC2ResourceDetails")
      )
