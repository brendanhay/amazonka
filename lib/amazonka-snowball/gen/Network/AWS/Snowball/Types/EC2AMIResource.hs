{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.EC2AMIResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.EC2AMIResource
  ( EC2AMIResource (..),

    -- * Smart constructor
    mkEC2AMIResource,

    -- * Lenses
    earSnowballAMIId,
    earAMIId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A JSON-formatted object that contains the IDs for an Amazon Machine Image (AMI), including the Amazon EC2 AMI ID and the Snow device AMI ID. Each AMI has these two IDs to simplify identifying the AMI in both the AWS Cloud and on the device.
--
-- /See:/ 'mkEC2AMIResource' smart constructor.
data EC2AMIResource = EC2AMIResource'
  { -- | The ID of the AMI on the Snow device.
    snowballAMIId :: Lude.Maybe Lude.Text,
    -- | The ID of the AMI in Amazon EC2.
    amiId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EC2AMIResource' with the minimum fields required to make a request.
--
-- * 'snowballAMIId' - The ID of the AMI on the Snow device.
-- * 'amiId' - The ID of the AMI in Amazon EC2.
mkEC2AMIResource ::
  -- | 'amiId'
  Lude.Text ->
  EC2AMIResource
mkEC2AMIResource pAMIId_ =
  EC2AMIResource' {snowballAMIId = Lude.Nothing, amiId = pAMIId_}

-- | The ID of the AMI on the Snow device.
--
-- /Note:/ Consider using 'snowballAMIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earSnowballAMIId :: Lens.Lens' EC2AMIResource (Lude.Maybe Lude.Text)
earSnowballAMIId = Lens.lens (snowballAMIId :: EC2AMIResource -> Lude.Maybe Lude.Text) (\s a -> s {snowballAMIId = a} :: EC2AMIResource)
{-# DEPRECATED earSnowballAMIId "Use generic-lens or generic-optics with 'snowballAMIId' instead." #-}

-- | The ID of the AMI in Amazon EC2.
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earAMIId :: Lens.Lens' EC2AMIResource Lude.Text
earAMIId = Lens.lens (amiId :: EC2AMIResource -> Lude.Text) (\s a -> s {amiId = a} :: EC2AMIResource)
{-# DEPRECATED earAMIId "Use generic-lens or generic-optics with 'amiId' instead." #-}

instance Lude.FromJSON EC2AMIResource where
  parseJSON =
    Lude.withObject
      "EC2AMIResource"
      ( \x ->
          EC2AMIResource'
            Lude.<$> (x Lude..:? "SnowballAmiId") Lude.<*> (x Lude..: "AmiId")
      )

instance Lude.ToJSON EC2AMIResource where
  toJSON EC2AMIResource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SnowballAmiId" Lude..=) Lude.<$> snowballAMIId,
            Lude.Just ("AmiId" Lude..= amiId)
          ]
      )
