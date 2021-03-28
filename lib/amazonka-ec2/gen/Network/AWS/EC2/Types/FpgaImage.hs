{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.FpgaImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.FpgaImage
  ( FpgaImage (..)
  -- * Smart constructor
  , mkFpgaImage
  -- * Lenses
  , fiCreateTime
  , fiDataRetentionSupport
  , fiDescription
  , fiFpgaImageGlobalId
  , fiFpgaImageId
  , fiName
  , fiOwnerAlias
  , fiOwnerId
  , fiPciId
  , fiProductCodes
  , fiPublic
  , fiShellVersion
  , fiState
  , fiTags
  , fiUpdateTime
  ) where

import qualified Network.AWS.EC2.Types.FpgaImageState as Types
import qualified Network.AWS.EC2.Types.PciId as Types
import qualified Network.AWS.EC2.Types.ProductCode as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Amazon FPGA image (AFI).
--
-- /See:/ 'mkFpgaImage' smart constructor.
data FpgaImage = FpgaImage'
  { createTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time the AFI was created.
  , dataRetentionSupport :: Core.Maybe Core.Bool
    -- ^ Indicates whether data retention support is enabled for the AFI.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the AFI.
  , fpgaImageGlobalId :: Core.Maybe Core.Text
    -- ^ The global FPGA image identifier (AGFI ID).
  , fpgaImageId :: Core.Maybe Core.Text
    -- ^ The FPGA image identifier (AFI ID).
  , name :: Core.Maybe Core.Text
    -- ^ The name of the AFI.
  , ownerAlias :: Core.Maybe Core.Text
    -- ^ The alias of the AFI owner. Possible values include @self@ , @amazon@ , and @aws-marketplace@ .
  , ownerId :: Core.Maybe Core.Text
    -- ^ The AWS account ID of the AFI owner.
  , pciId :: Core.Maybe Types.PciId
    -- ^ Information about the PCI bus.
  , productCodes :: Core.Maybe [Types.ProductCode]
    -- ^ The product codes for the AFI.
  , public :: Core.Maybe Core.Bool
    -- ^ Indicates whether the AFI is public.
  , shellVersion :: Core.Maybe Core.Text
    -- ^ The version of the AWS Shell that was used to create the bitstream.
  , state :: Core.Maybe Types.FpgaImageState
    -- ^ Information about the state of the AFI.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the AFI.
  , updateTime :: Core.Maybe Core.UTCTime
    -- ^ The time of the most recent update to the AFI.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'FpgaImage' value with any optional fields omitted.
mkFpgaImage
    :: FpgaImage
mkFpgaImage
  = FpgaImage'{createTime = Core.Nothing,
               dataRetentionSupport = Core.Nothing, description = Core.Nothing,
               fpgaImageGlobalId = Core.Nothing, fpgaImageId = Core.Nothing,
               name = Core.Nothing, ownerAlias = Core.Nothing,
               ownerId = Core.Nothing, pciId = Core.Nothing,
               productCodes = Core.Nothing, public = Core.Nothing,
               shellVersion = Core.Nothing, state = Core.Nothing,
               tags = Core.Nothing, updateTime = Core.Nothing}

-- | The date and time the AFI was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiCreateTime :: Lens.Lens' FpgaImage (Core.Maybe Core.UTCTime)
fiCreateTime = Lens.field @"createTime"
{-# INLINEABLE fiCreateTime #-}
{-# DEPRECATED createTime "Use generic-lens or generic-optics with 'createTime' instead"  #-}

-- | Indicates whether data retention support is enabled for the AFI.
--
-- /Note:/ Consider using 'dataRetentionSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiDataRetentionSupport :: Lens.Lens' FpgaImage (Core.Maybe Core.Bool)
fiDataRetentionSupport = Lens.field @"dataRetentionSupport"
{-# INLINEABLE fiDataRetentionSupport #-}
{-# DEPRECATED dataRetentionSupport "Use generic-lens or generic-optics with 'dataRetentionSupport' instead"  #-}

-- | The description of the AFI.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiDescription :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fiDescription = Lens.field @"description"
{-# INLINEABLE fiDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The global FPGA image identifier (AGFI ID).
--
-- /Note:/ Consider using 'fpgaImageGlobalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiFpgaImageGlobalId :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fiFpgaImageGlobalId = Lens.field @"fpgaImageGlobalId"
{-# INLINEABLE fiFpgaImageGlobalId #-}
{-# DEPRECATED fpgaImageGlobalId "Use generic-lens or generic-optics with 'fpgaImageGlobalId' instead"  #-}

-- | The FPGA image identifier (AFI ID).
--
-- /Note:/ Consider using 'fpgaImageId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiFpgaImageId :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fiFpgaImageId = Lens.field @"fpgaImageId"
{-# INLINEABLE fiFpgaImageId #-}
{-# DEPRECATED fpgaImageId "Use generic-lens or generic-optics with 'fpgaImageId' instead"  #-}

-- | The name of the AFI.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiName :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fiName = Lens.field @"name"
{-# INLINEABLE fiName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The alias of the AFI owner. Possible values include @self@ , @amazon@ , and @aws-marketplace@ .
--
-- /Note:/ Consider using 'ownerAlias' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiOwnerAlias :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fiOwnerAlias = Lens.field @"ownerAlias"
{-# INLINEABLE fiOwnerAlias #-}
{-# DEPRECATED ownerAlias "Use generic-lens or generic-optics with 'ownerAlias' instead"  #-}

-- | The AWS account ID of the AFI owner.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiOwnerId :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fiOwnerId = Lens.field @"ownerId"
{-# INLINEABLE fiOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | Information about the PCI bus.
--
-- /Note:/ Consider using 'pciId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiPciId :: Lens.Lens' FpgaImage (Core.Maybe Types.PciId)
fiPciId = Lens.field @"pciId"
{-# INLINEABLE fiPciId #-}
{-# DEPRECATED pciId "Use generic-lens or generic-optics with 'pciId' instead"  #-}

-- | The product codes for the AFI.
--
-- /Note:/ Consider using 'productCodes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiProductCodes :: Lens.Lens' FpgaImage (Core.Maybe [Types.ProductCode])
fiProductCodes = Lens.field @"productCodes"
{-# INLINEABLE fiProductCodes #-}
{-# DEPRECATED productCodes "Use generic-lens or generic-optics with 'productCodes' instead"  #-}

-- | Indicates whether the AFI is public.
--
-- /Note:/ Consider using 'public' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiPublic :: Lens.Lens' FpgaImage (Core.Maybe Core.Bool)
fiPublic = Lens.field @"public"
{-# INLINEABLE fiPublic #-}
{-# DEPRECATED public "Use generic-lens or generic-optics with 'public' instead"  #-}

-- | The version of the AWS Shell that was used to create the bitstream.
--
-- /Note:/ Consider using 'shellVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiShellVersion :: Lens.Lens' FpgaImage (Core.Maybe Core.Text)
fiShellVersion = Lens.field @"shellVersion"
{-# INLINEABLE fiShellVersion #-}
{-# DEPRECATED shellVersion "Use generic-lens or generic-optics with 'shellVersion' instead"  #-}

-- | Information about the state of the AFI.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiState :: Lens.Lens' FpgaImage (Core.Maybe Types.FpgaImageState)
fiState = Lens.field @"state"
{-# INLINEABLE fiState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Any tags assigned to the AFI.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiTags :: Lens.Lens' FpgaImage (Core.Maybe [Types.Tag])
fiTags = Lens.field @"tags"
{-# INLINEABLE fiTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The time of the most recent update to the AFI.
--
-- /Note:/ Consider using 'updateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fiUpdateTime :: Lens.Lens' FpgaImage (Core.Maybe Core.UTCTime)
fiUpdateTime = Lens.field @"updateTime"
{-# INLINEABLE fiUpdateTime #-}
{-# DEPRECATED updateTime "Use generic-lens or generic-optics with 'updateTime' instead"  #-}

instance Core.FromXML FpgaImage where
        parseXML x
          = FpgaImage' Core.<$>
              (x Core..@? "createTime") Core.<*>
                x Core..@? "dataRetentionSupport"
                Core.<*> x Core..@? "description"
                Core.<*> x Core..@? "fpgaImageGlobalId"
                Core.<*> x Core..@? "fpgaImageId"
                Core.<*> x Core..@? "name"
                Core.<*> x Core..@? "ownerAlias"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "pciId"
                Core.<*>
                x Core..@? "productCodes" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "public"
                Core.<*> x Core..@? "shellVersion"
                Core.<*> x Core..@? "state"
                Core.<*> x Core..@? "tags" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "updateTime"
