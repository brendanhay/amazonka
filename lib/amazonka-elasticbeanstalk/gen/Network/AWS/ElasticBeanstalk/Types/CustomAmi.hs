{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.CustomAmi
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ElasticBeanstalk.Types.CustomAmi
  ( CustomAmi (..)
  -- * Smart constructor
  , mkCustomAmi
  -- * Lenses
  , caImageId
  , caVirtualizationType
  ) where

import qualified Network.AWS.ElasticBeanstalk.Types.ImageId as Types
import qualified Network.AWS.ElasticBeanstalk.Types.VirtualizationType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A custom AMI available to platforms.
--
-- /See:/ 'mkCustomAmi' smart constructor.
data CustomAmi = CustomAmi'
  { imageId :: Core.Maybe Types.ImageId
    -- ^ THe ID of the image used to create the custom AMI.
  , virtualizationType :: Core.Maybe Types.VirtualizationType
    -- ^ The type of virtualization used to create the custom AMI.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CustomAmi' value with any optional fields omitted.
mkCustomAmi
    :: CustomAmi
mkCustomAmi
  = CustomAmi'{imageId = Core.Nothing,
               virtualizationType = Core.Nothing}

-- | THe ID of the image used to create the custom AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caImageId :: Lens.Lens' CustomAmi (Core.Maybe Types.ImageId)
caImageId = Lens.field @"imageId"
{-# INLINEABLE caImageId #-}
{-# DEPRECATED imageId "Use generic-lens or generic-optics with 'imageId' instead"  #-}

-- | The type of virtualization used to create the custom AMI.
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caVirtualizationType :: Lens.Lens' CustomAmi (Core.Maybe Types.VirtualizationType)
caVirtualizationType = Lens.field @"virtualizationType"
{-# INLINEABLE caVirtualizationType #-}
{-# DEPRECATED virtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead"  #-}

instance Core.FromXML CustomAmi where
        parseXML x
          = CustomAmi' Core.<$>
              (x Core..@? "ImageId") Core.<*> x Core..@? "VirtualizationType"
