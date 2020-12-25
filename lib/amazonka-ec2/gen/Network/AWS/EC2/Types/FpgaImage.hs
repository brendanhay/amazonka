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
    fiCreateTime,
    fiDataRetentionSupport,
    fiDescription,
    fiFpgaImageGlobalId,
    fiFpgaImageId,
    fiName,
    fiOwnerAlias,
    fiOwnerId,
    fiPciId,
    fiProductCodes,
    fiPublic,
    fiShellVersion,
    fiState,
    fiTags,
    fiUpdateTime,
  )
where

import qualified Network.AWS.EC2.Types.FpgaImageState as Types
import qualified Network.AWS.EC2.Types.PciId as Types
import qualified Network.AWS.EC2.Types.ProductCode as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon FPGA image (AFI).
--
-- /See:/ 'mkFpgaImage' smart constructor.
data FpgaImage = FpgaImage'
  { -- | The date and time the AFI was created.
    createTime :: Core.Maybe Core.UTCTime,
    -- | Indicates whether data retention support is enabled for the AFI.
    dataRetentionSupport :: Core.Maybe Core.Bool,
    -- | The description of the AFI.
    description :: Core.Maybe Types.String,
    -- | The global FPGA image identifier (AGFI ID).
    fpgaImageGlobalId :: Core.Maybe Types.String,
    -- | The FPGA image identifier (AFI ID).
    fpgaImageId :: Core.Maybe Types.String,
    -- | The name of the AFI.
    name :: Core.Maybe Types.String,
    -- | The alias of the AFI owner. Possible values include @self@ , @amazon@ , and @aws-marketplace@ .
    ownerAlias :: Core.Maybe Types.String,
    -- | The AWS account ID of the AFI owner.
    ownerId :: Core.Maybe Types.String,
    -- | Information about the PCI bus.
    pciId :: Core.Maybe Types.PciId,
    -- | The product codes for the AFI.
    productCodes :: Core.Maybe [Types.ProductCode],
    -- | Indicates whether the AFI is public.
    public :: Core.Maybe Core.Bool,
    -- | The version of the AWS Shell that was used to create the bitstream.
    shellVersion :: Core.Maybe Types.String,
    -- | Information about the state of the AFI.
    state :: Core.Maybe Types.FpgaImageState,
    -- | Any tags assigned to the AFI.
    tags :: Core.Maybe [Types.Tag],
    -- | The time of the most recent update to the AFI.
    updateTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'FpgaImage' value with any optional fields omitted.
mkFpgaImage ::
  FpgaImage
mkFpgaImage =
  FpgaImage'
    { createTime = Core.Nothing,
      dataRetentionSupport = Core.Nothing,
      description = Core.Nothing,
      fpgaImageGlobalId = Core.Nothing,
      fpgaImageId = Core.Nothing,
      name = Core.Nothing,
      ownerAlias = Core.Nothing,
      ownerId = Core.Nothing,
      pciId = Core.Nothing,
      productCodes = Core.Nothing,
      public = Core.Nothing,
      shellVersion = Core.Nothing,
      state = Core.Nothing,
      tags = Core.Nothing,
      updateTime = Core.Nothing
    }

-- | The date and time the AFI was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiCreateTime :: Lens.Lens' FpgaImage (Core.Maybe Core.UTCTime)
fiCreateTime = Lens.field @"createTime"
{-# DEPRECATED fiCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | Indicates whether data retention support is enabled for the AFI.
--
-- /Note:/ Consider using 'dataRetentionSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiDataRetentionSupport :: Lens.Lens' FpgaImage (Core.Maybe Core.Bool)
fiDataRetentionSupport = Lens.field @"dataRetentionSupport"
{-# DEPRECATED fiDataRetentionSupport "Use generic-lens or generic-optics with 'dataRetentionSupport' instead." #-}

-- | The description of the AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiDescription :: Lens.Lens' FpgaImage (Core.Maybe Types.String)
fiDescription = Lens.field @"description"
{-# DEPRECATED fiDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The global FPGA image identifier (AGFI ID).
--
-- /Note:/ Consider using 'fpgaImageGlobalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiFpgaImageGlobalId :: Lens.Lens' FpgaImage (Core.Maybe Types.String)
fiFpgaImageGlobalId = Lens.field @"fpgaImageGlobalId"
{-# DEPRECATED fiFpgaImageGlobalId "Use generic-lens or generic-optics with 'fpgaImageGlobalId' instead." #-}

-- | The FPGA image identifier (AFI ID).
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiFpgaImageId :: Lens.Lens' FpgaImage (Core.Maybe Types.String)
fiFpgaImageId = Lens.field @"fpgaImageId"
{-# DEPRECATED fiFpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead." #-}

-- | The name of the AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiName :: Lens.Lens' FpgaImage (Core.Maybe Types.String)
fiName = Lens.field @"name"
{-# DEPRECATED fiName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The alias of the AFI owner. Possible values include @self@ , @amazon@ , and @aws-marketplace@ .
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiOwnerAlias :: Lens.Lens' FpgaImage (Core.Maybe Types.String)
fiOwnerAlias = Lens.field @"ownerAlias"
{-# DEPRECATED fiOwnerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead." #-}

-- | The AWS account ID of the AFI owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiOwnerId :: Lens.Lens' FpgaImage (Core.Maybe Types.String)
fiOwnerId = Lens.field @"ownerId"
{-# DEPRECATED fiOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | Information about the PCI bus.
--
-- /Note:/ Consider using 'pciId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiPciId :: Lens.Lens' FpgaImage (Core.Maybe Types.PciId)
fiPciId = Lens.field @"pciId"
{-# DEPRECATED fiPciId "Use generic-lens or generic-optics with 'pciId' instead." #-}

-- | The product codes for the AFI.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiProductCodes :: Lens.Lens' FpgaImage (Core.Maybe [Types.ProductCode])
fiProductCodes = Lens.field @"productCodes"
{-# DEPRECATED fiProductCodes "Use generic-lens or generic-optics with 'productCodes' instead." #-}

-- | Indicates whether the AFI is public.
--
-- /Note:/ Consider using 'public' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiPublic :: Lens.Lens' FpgaImage (Core.Maybe Core.Bool)
fiPublic = Lens.field @"public"
{-# DEPRECATED fiPublic "Use generic-lens or generic-optics with 'public' instead." #-}

-- | The version of the AWS Shell that was used to create the bitstream.
--
-- /Note:/ Consider using 'shellVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiShellVersion :: Lens.Lens' FpgaImage (Core.Maybe Types.String)
fiShellVersion = Lens.field @"shellVersion"
{-# DEPRECATED fiShellVersion "Use generic-lens or generic-optics with 'shellVersion' instead." #-}

-- | Information about the state of the AFI.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiState :: Lens.Lens' FpgaImage (Core.Maybe Types.FpgaImageState)
fiState = Lens.field @"state"
{-# DEPRECATED fiState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | Any tags assigned to the AFI.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiTags :: Lens.Lens' FpgaImage (Core.Maybe [Types.Tag])
fiTags = Lens.field @"tags"
{-# DEPRECATED fiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The time of the most recent update to the AFI.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiUpdateTime :: Lens.Lens' FpgaImage (Core.Maybe Core.UTCTime)
fiUpdateTime = Lens.field @"updateTime"
{-# DEPRECATED fiUpdateTime "Use generic-lens or generic-optics with 'updateTime' instead." #-}

instance Core.FromXML FpgaImage where
  parseXML x =
    FpgaImage'
      Core.<$> (x Core..@? "createTime")
      Core.<*> (x Core..@? "dataRetentionSupport")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "fpgaImageGlobalId")
      Core.<*> (x Core..@? "fpgaImageId")
      Core.<*> (x Core..@? "name")
      Core.<*> (x Core..@? "ownerAlias")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "pciId")
      Core.<*> (x Core..@? "productCodes" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "public")
      Core.<*> (x Core..@? "shellVersion")
      Core.<*> (x Core..@? "state")
      Core.<*> (x Core..@? "tags" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "updateTime")
