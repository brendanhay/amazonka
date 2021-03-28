{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EbsInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.EbsInfo
  ( EbsInfo (..)
  -- * Smart constructor
  , mkEbsInfo
  -- * Lenses
  , eiEbsOptimizedInfo
  , eiEbsOptimizedSupport
  , eiEncryptionSupport
  , eiNvmeSupport
  ) where

import qualified Network.AWS.EC2.Types.EbsEncryptionSupport as Types
import qualified Network.AWS.EC2.Types.EbsNvmeSupport as Types
import qualified Network.AWS.EC2.Types.EbsOptimizedInfo as Types
import qualified Network.AWS.EC2.Types.EbsOptimizedSupport as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon EBS features supported by the instance type.
--
-- /See:/ 'mkEbsInfo' smart constructor.
data EbsInfo = EbsInfo'
  { ebsOptimizedInfo :: Core.Maybe Types.EbsOptimizedInfo
    -- ^ Describes the optimized EBS performance for the instance type.
  , ebsOptimizedSupport :: Core.Maybe Types.EbsOptimizedSupport
    -- ^ Indicates whether the instance type is Amazon EBS-optimized. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in /Amazon EC2 User Guide for Linux Instances/ .
  , encryptionSupport :: Core.Maybe Types.EbsEncryptionSupport
    -- ^ Indicates whether Amazon EBS encryption is supported.
  , nvmeSupport :: Core.Maybe Types.EbsNvmeSupport
    -- ^ Indicates whether non-volatile memory express (NVMe) is supported.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EbsInfo' value with any optional fields omitted.
mkEbsInfo
    :: EbsInfo
mkEbsInfo
  = EbsInfo'{ebsOptimizedInfo = Core.Nothing,
             ebsOptimizedSupport = Core.Nothing,
             encryptionSupport = Core.Nothing, nvmeSupport = Core.Nothing}

-- | Describes the optimized EBS performance for the instance type.
--
-- /Note:/ Consider using 'ebsOptimizedInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiEbsOptimizedInfo :: Lens.Lens' EbsInfo (Core.Maybe Types.EbsOptimizedInfo)
eiEbsOptimizedInfo = Lens.field @"ebsOptimizedInfo"
{-# INLINEABLE eiEbsOptimizedInfo #-}
{-# DEPRECATED ebsOptimizedInfo "Use generic-lens or generic-optics with 'ebsOptimizedInfo' instead"  #-}

-- | Indicates whether the instance type is Amazon EBS-optimized. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'ebsOptimizedSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiEbsOptimizedSupport :: Lens.Lens' EbsInfo (Core.Maybe Types.EbsOptimizedSupport)
eiEbsOptimizedSupport = Lens.field @"ebsOptimizedSupport"
{-# INLINEABLE eiEbsOptimizedSupport #-}
{-# DEPRECATED ebsOptimizedSupport "Use generic-lens or generic-optics with 'ebsOptimizedSupport' instead"  #-}

-- | Indicates whether Amazon EBS encryption is supported.
--
-- /Note:/ Consider using 'encryptionSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiEncryptionSupport :: Lens.Lens' EbsInfo (Core.Maybe Types.EbsEncryptionSupport)
eiEncryptionSupport = Lens.field @"encryptionSupport"
{-# INLINEABLE eiEncryptionSupport #-}
{-# DEPRECATED encryptionSupport "Use generic-lens or generic-optics with 'encryptionSupport' instead"  #-}

-- | Indicates whether non-volatile memory express (NVMe) is supported.
--
-- /Note:/ Consider using 'nvmeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiNvmeSupport :: Lens.Lens' EbsInfo (Core.Maybe Types.EbsNvmeSupport)
eiNvmeSupport = Lens.field @"nvmeSupport"
{-# INLINEABLE eiNvmeSupport #-}
{-# DEPRECATED nvmeSupport "Use generic-lens or generic-optics with 'nvmeSupport' instead"  #-}

instance Core.FromXML EbsInfo where
        parseXML x
          = EbsInfo' Core.<$>
              (x Core..@? "ebsOptimizedInfo") Core.<*>
                x Core..@? "ebsOptimizedSupport"
                Core.<*> x Core..@? "encryptionSupport"
                Core.<*> x Core..@? "nvmeSupport"
