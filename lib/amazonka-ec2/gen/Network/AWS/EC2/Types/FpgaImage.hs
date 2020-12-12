{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.FpgaImage
  ( FpgaImage (..),

    -- * Smart constructor
    mkFpgaImage,

    -- * Lenses
    fiShellVersion,
    fiPciId,
    fiState,
    fiOwnerAlias,
    fiFpgaImageId,
    fiDataRetentionSupport,
    fiOwnerId,
    fiUpdateTime,
    fiName,
    fiProductCodes,
    fiDescription,
    fiCreateTime,
    fiTags,
    fiPublic,
    fiFpgaImageGlobalId,
  )
where

import Network.AWS.EC2.Types.FpgaImageState
import Network.AWS.EC2.Types.PciId
import Network.AWS.EC2.Types.ProductCode
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an Amazon FPGA image (AFI).
--
-- /See:/ 'mkFpgaImage' smart constructor.
data FpgaImage = FpgaImage'
  { shellVersion :: Lude.Maybe Lude.Text,
    pciId :: Lude.Maybe PciId,
    state :: Lude.Maybe FpgaImageState,
    ownerAlias :: Lude.Maybe Lude.Text,
    fpgaImageId :: Lude.Maybe Lude.Text,
    dataRetentionSupport :: Lude.Maybe Lude.Bool,
    ownerId :: Lude.Maybe Lude.Text,
    updateTime :: Lude.Maybe Lude.DateTime,
    name :: Lude.Maybe Lude.Text,
    productCodes :: Lude.Maybe [ProductCode],
    description :: Lude.Maybe Lude.Text,
    createTime :: Lude.Maybe Lude.DateTime,
    tags :: Lude.Maybe [Tag],
    public :: Lude.Maybe Lude.Bool,
    fpgaImageGlobalId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FpgaImage' with the minimum fields required to make a request.
--
-- * 'createTime' - The date and time the AFI was created.
-- * 'dataRetentionSupport' - Indicates whether data retention support is enabled for the AFI.
-- * 'description' - The description of the AFI.
-- * 'fpgaImageGlobalId' - The global FPGA image identifier (AGFI ID).
-- * 'fpgaImageId' - The FPGA image identifier (AFI ID).
-- * 'name' - The name of the AFI.
-- * 'ownerAlias' - The alias of the AFI owner. Possible values include @self@ , @amazon@ , and @aws-marketplace@ .
-- * 'ownerId' - The AWS account ID of the AFI owner.
-- * 'pciId' - Information about the PCI bus.
-- * 'productCodes' - The product codes for the AFI.
-- * 'public' - Indicates whether the AFI is public.
-- * 'shellVersion' - The version of the AWS Shell that was used to create the bitstream.
-- * 'state' - Information about the state of the AFI.
-- * 'tags' - Any tags assigned to the AFI.
-- * 'updateTime' - The time of the most recent update to the AFI.
mkFpgaImage ::
  FpgaImage
mkFpgaImage =
  FpgaImage'
    { shellVersion = Lude.Nothing,
      pciId = Lude.Nothing,
      state = Lude.Nothing,
      ownerAlias = Lude.Nothing,
      fpgaImageId = Lude.Nothing,
      dataRetentionSupport = Lude.Nothing,
      ownerId = Lude.Nothing,
      updateTime = Lude.Nothing,
      name = Lude.Nothing,
      productCodes = Lude.Nothing,
      description = Lude.Nothing,
      createTime = Lude.Nothing,
      tags = Lude.Nothing,
      public = Lude.Nothing,
      fpgaImageGlobalId = Lude.Nothing
    }

-- | The version of the AWS Shell that was used to create the bitstream.
--
-- /Note:/ Consider using 'shellVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiShellVersion :: Lens.Lens' FpgaImage (Lude.Maybe Lude.Text)
fiShellVersion = Lens.lens (shellVersion :: FpgaImage -> Lude.Maybe Lude.Text) (\s a -> s {shellVersion = a} :: FpgaImage)
{-# DEPRECATED fiShellVersion "Use generic-lens or generic-optics with 'shellVersion' instead." #-}

-- | Information about the PCI bus.
--
-- /Note:/ Consider using 'pciId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiPciId :: Lens.Lens' FpgaImage (Lude.Maybe PciId)
fiPciId = Lens.lens (pciId :: FpgaImage -> Lude.Maybe PciId) (\s a -> s {pciId = a} :: FpgaImage)
{-# DEPRECATED fiPciId "Use generic-lens or generic-optics with 'pciId' instead." #-}

-- | Information about the state of the AFI.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiState :: Lens.Lens' FpgaImage (Lude.Maybe FpgaImageState)
fiState = Lens.lens (state :: FpgaImage -> Lude.Maybe FpgaImageState) (\s a -> s {state = a} :: FpgaImage)
{-# DEPRECATED fiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The alias of the AFI owner. Possible values include @self@ , @amazon@ , and @aws-marketplace@ .
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiOwnerAlias :: Lens.Lens' FpgaImage (Lude.Maybe Lude.Text)
fiOwnerAlias = Lens.lens (ownerAlias :: FpgaImage -> Lude.Maybe Lude.Text) (\s a -> s {ownerAlias = a} :: FpgaImage)
{-# DEPRECATED fiOwnerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead." #-}

-- | The FPGA image identifier (AFI ID).
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiFpgaImageId :: Lens.Lens' FpgaImage (Lude.Maybe Lude.Text)
fiFpgaImageId = Lens.lens (fpgaImageId :: FpgaImage -> Lude.Maybe Lude.Text) (\s a -> s {fpgaImageId = a} :: FpgaImage)
{-# DEPRECATED fiFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | Indicates whether data retention support is enabled for the AFI.
--
-- /Note:/ Consider using 'dataRetentionSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiDataRetentionSupport :: Lens.Lens' FpgaImage (Lude.Maybe Lude.Bool)
fiDataRetentionSupport = Lens.lens (dataRetentionSupport :: FpgaImage -> Lude.Maybe Lude.Bool) (\s a -> s {dataRetentionSupport = a} :: FpgaImage)
{-# DEPRECATED fiDataRetentionSupport "Use generic-lens or generic-optics with 'dataRetentionSupport' instead." #-}

-- | The AWS account ID of the AFI owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiOwnerId :: Lens.Lens' FpgaImage (Lude.Maybe Lude.Text)
fiOwnerId = Lens.lens (ownerId :: FpgaImage -> Lude.Maybe Lude.Text) (\s a -> s {ownerId = a} :: FpgaImage)
{-# DEPRECATED fiOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The time of the most recent update to the AFI.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiUpdateTime :: Lens.Lens' FpgaImage (Lude.Maybe Lude.DateTime)
fiUpdateTime = Lens.lens (updateTime :: FpgaImage -> Lude.Maybe Lude.DateTime) (\s a -> s {updateTime = a} :: FpgaImage)
{-# DEPRECATED fiUpdateTime "Use generic-lens or generic-optics with 'updateTime' instead." #-}

-- | The name of the AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiName :: Lens.Lens' FpgaImage (Lude.Maybe Lude.Text)
fiName = Lens.lens (name :: FpgaImage -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: FpgaImage)
{-# DEPRECATED fiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The product codes for the AFI.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiProductCodes :: Lens.Lens' FpgaImage (Lude.Maybe [ProductCode])
fiProductCodes = Lens.lens (productCodes :: FpgaImage -> Lude.Maybe [ProductCode]) (\s a -> s {productCodes = a} :: FpgaImage)
{-# DEPRECATED fiProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | The description of the AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiDescription :: Lens.Lens' FpgaImage (Lude.Maybe Lude.Text)
fiDescription = Lens.lens (description :: FpgaImage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: FpgaImage)
{-# DEPRECATED fiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date and time the AFI was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiCreateTime :: Lens.Lens' FpgaImage (Lude.Maybe Lude.DateTime)
fiCreateTime = Lens.lens (createTime :: FpgaImage -> Lude.Maybe Lude.DateTime) (\s a -> s {createTime = a} :: FpgaImage)
{-# DEPRECATED fiCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | Any tags assigned to the AFI.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiTags :: Lens.Lens' FpgaImage (Lude.Maybe [Tag])
fiTags = Lens.lens (tags :: FpgaImage -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: FpgaImage)
{-# DEPRECATED fiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Indicates whether the AFI is public.
--
-- /Note:/ Consider using 'public' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiPublic :: Lens.Lens' FpgaImage (Lude.Maybe Lude.Bool)
fiPublic = Lens.lens (public :: FpgaImage -> Lude.Maybe Lude.Bool) (\s a -> s {public = a} :: FpgaImage)
{-# DEPRECATED fiPublic "Use generic-lens or generic-optics with 'public' instead." #-}

-- | The global FPGA image identifier (AGFI ID).
--
-- /Note:/ Consider using 'fpgaImageGlobalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiFpgaImageGlobalId :: Lens.Lens' FpgaImage (Lude.Maybe Lude.Text)
fiFpgaImageGlobalId = Lens.lens (fpgaImageGlobalId :: FpgaImage -> Lude.Maybe Lude.Text) (\s a -> s {fpgaImageGlobalId = a} :: FpgaImage)
{-# DEPRECATED fiFpgaImageGlobalId "Use generic-lens or generic-optics with 'fpgaImageGlobalId' instead." #-}

instance Lude.FromXML FpgaImage where
  parseXML x =
    FpgaImage'
      Lude.<$> (x Lude..@? "shellVersion")
      Lude.<*> (x Lude..@? "pciId")
      Lude.<*> (x Lude..@? "state")
      Lude.<*> (x Lude..@? "ownerAlias")
      Lude.<*> (x Lude..@? "fpgaImageId")
      Lude.<*> (x Lude..@? "dataRetentionSupport")
      Lude.<*> (x Lude..@? "ownerId")
      Lude.<*> (x Lude..@? "updateTime")
      Lude.<*> (x Lude..@? "name")
      Lude.<*> ( x Lude..@? "productCodes" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "createTime")
      Lude.<*> ( x Lude..@? "tags" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "public")
      Lude.<*> (x Lude..@? "fpgaImageGlobalId")
