{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.CustomAMI
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.CustomAMI
  ( CustomAMI (..),

    -- * Smart constructor
    mkCustomAMI,

    -- * Lenses
    caVirtualizationType,
    caImageId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A custom AMI available to platforms.
--
-- /See:/ 'mkCustomAMI' smart constructor.
data CustomAMI = CustomAMI'
  { virtualizationType ::
      Lude.Maybe Lude.Text,
    imageId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomAMI' with the minimum fields required to make a request.
--
-- * 'imageId' - THe ID of the image used to create the custom AMI.
-- * 'virtualizationType' - The type of virtualization used to create the custom AMI.
mkCustomAMI ::
  CustomAMI
mkCustomAMI =
  CustomAMI'
    { virtualizationType = Lude.Nothing,
      imageId = Lude.Nothing
    }

-- | The type of virtualization used to create the custom AMI.
--
-- /Note:/ Consider using 'virtualizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caVirtualizationType :: Lens.Lens' CustomAMI (Lude.Maybe Lude.Text)
caVirtualizationType = Lens.lens (virtualizationType :: CustomAMI -> Lude.Maybe Lude.Text) (\s a -> s {virtualizationType = a} :: CustomAMI)
{-# DEPRECATED caVirtualizationType "Use generic-lens or generic-optics with 'virtualizationType' instead." #-}

-- | THe ID of the image used to create the custom AMI.
--
-- /Note:/ Consider using 'imageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caImageId :: Lens.Lens' CustomAMI (Lude.Maybe Lude.Text)
caImageId = Lens.lens (imageId :: CustomAMI -> Lude.Maybe Lude.Text) (\s a -> s {imageId = a} :: CustomAMI)
{-# DEPRECATED caImageId "Use generic-lens or generic-optics with 'imageId' instead." #-}

instance Lude.FromXML CustomAMI where
  parseXML x =
    CustomAMI'
      Lude.<$> (x Lude..@? "VirtualizationType") Lude.<*> (x Lude..@? "ImageId")
