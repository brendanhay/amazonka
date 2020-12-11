-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.CompatibleImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.CompatibleImage
  ( CompatibleImage (..),

    -- * Smart constructor
    mkCompatibleImage,

    -- * Lenses
    ciName,
    ciAMIId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A JSON-formatted object that describes a compatible Amazon Machine Image (AMI), including the ID and name for a Snow device AMI. This AMI is compatible with the device's physical hardware requirements, and it should be able to be run in an SBE1 instance on the device.
--
-- /See:/ 'mkCompatibleImage' smart constructor.
data CompatibleImage = CompatibleImage'
  { name ::
      Lude.Maybe Lude.Text,
    amiId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CompatibleImage' with the minimum fields required to make a request.
--
-- * 'amiId' - The unique identifier for an individual Snow device AMI.
-- * 'name' - The optional name of a compatible image.
mkCompatibleImage ::
  CompatibleImage
mkCompatibleImage =
  CompatibleImage' {name = Lude.Nothing, amiId = Lude.Nothing}

-- | The optional name of a compatible image.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciName :: Lens.Lens' CompatibleImage (Lude.Maybe Lude.Text)
ciName = Lens.lens (name :: CompatibleImage -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CompatibleImage)
{-# DEPRECATED ciName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The unique identifier for an individual Snow device AMI.
--
-- /Note:/ Consider using 'amiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciAMIId :: Lens.Lens' CompatibleImage (Lude.Maybe Lude.Text)
ciAMIId = Lens.lens (amiId :: CompatibleImage -> Lude.Maybe Lude.Text) (\s a -> s {amiId = a} :: CompatibleImage)
{-# DEPRECATED ciAMIId "Use generic-lens or generic-optics with 'amiId' instead." #-}

instance Lude.FromJSON CompatibleImage where
  parseJSON =
    Lude.withObject
      "CompatibleImage"
      ( \x ->
          CompatibleImage'
            Lude.<$> (x Lude..:? "Name") Lude.<*> (x Lude..:? "AmiId")
      )
