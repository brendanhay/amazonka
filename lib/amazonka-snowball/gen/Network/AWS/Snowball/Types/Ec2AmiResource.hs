{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.Ec2AmiResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.Ec2AmiResource
  ( Ec2AmiResource (..)
  -- * Smart constructor
  , mkEc2AmiResource
  -- * Lenses
  , earAmiId
  , earSnowballAmiId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.AmiId as Types

-- | A JSON-formatted object that contains the IDs for an Amazon Machine Image (AMI), including the Amazon EC2 AMI ID and the Snow device AMI ID. Each AMI has these two IDs to simplify identifying the AMI in both the AWS Cloud and on the device.
--
-- /See:/ 'mkEc2AmiResource' smart constructor.
data Ec2AmiResource = Ec2AmiResource'
  { amiId :: Types.AmiId
    -- ^ The ID of the AMI in Amazon EC2.
  , snowballAmiId :: Core.Maybe Core.Text
    -- ^ The ID of the AMI on the Snow device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Ec2AmiResource' value with any optional fields omitted.
mkEc2AmiResource
    :: Types.AmiId -- ^ 'amiId'
    -> Ec2AmiResource
mkEc2AmiResource amiId
  = Ec2AmiResource'{amiId, snowballAmiId = Core.Nothing}

-- | The ID of the AMI in Amazon EC2.
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earAmiId :: Lens.Lens' Ec2AmiResource Types.AmiId
earAmiId = Lens.field @"amiId"
{-# INLINEABLE earAmiId #-}
{-# DEPRECATED amiId "Use generic-lens or generic-optics with 'amiId' instead"  #-}

-- | The ID of the AMI on the Snow device.
--
-- /Note:/ Consider using 'snowballAmiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earSnowballAmiId :: Lens.Lens' Ec2AmiResource (Core.Maybe Core.Text)
earSnowballAmiId = Lens.field @"snowballAmiId"
{-# INLINEABLE earSnowballAmiId #-}
{-# DEPRECATED snowballAmiId "Use generic-lens or generic-optics with 'snowballAmiId' instead"  #-}

instance Core.FromJSON Ec2AmiResource where
        toJSON Ec2AmiResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AmiId" Core..= amiId),
                  ("SnowballAmiId" Core..=) Core.<$> snowballAmiId])

instance Core.FromJSON Ec2AmiResource where
        parseJSON
          = Core.withObject "Ec2AmiResource" Core.$
              \ x ->
                Ec2AmiResource' Core.<$>
                  (x Core..: "AmiId") Core.<*> x Core..:? "SnowballAmiId"
