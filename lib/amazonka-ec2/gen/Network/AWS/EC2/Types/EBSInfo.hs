{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.EBSInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.EBSInfo
  ( EBSInfo (..),

    -- * Smart constructor
    mkEBSInfo,

    -- * Lenses
    eiEBSOptimizedInfo,
    eiEncryptionSupport,
    eiEBSOptimizedSupport,
    eiNvmeSupport,
  )
where

import Network.AWS.EC2.Types.EBSEncryptionSupport
import Network.AWS.EC2.Types.EBSNvmeSupport
import Network.AWS.EC2.Types.EBSOptimizedInfo
import Network.AWS.EC2.Types.EBSOptimizedSupport
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the Amazon EBS features supported by the instance type.
--
-- /See:/ 'mkEBSInfo' smart constructor.
data EBSInfo = EBSInfo'
  { ebsOptimizedInfo ::
      Lude.Maybe EBSOptimizedInfo,
    encryptionSupport :: Lude.Maybe EBSEncryptionSupport,
    ebsOptimizedSupport :: Lude.Maybe EBSOptimizedSupport,
    nvmeSupport :: Lude.Maybe EBSNvmeSupport
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EBSInfo' with the minimum fields required to make a request.
--
-- * 'ebsOptimizedInfo' - Describes the optimized EBS performance for the instance type.
-- * 'ebsOptimizedSupport' - Indicates whether the instance type is Amazon EBS-optimized. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in /Amazon EC2 User Guide for Linux Instances/ .
-- * 'encryptionSupport' - Indicates whether Amazon EBS encryption is supported.
-- * 'nvmeSupport' - Indicates whether non-volatile memory express (NVMe) is supported.
mkEBSInfo ::
  EBSInfo
mkEBSInfo =
  EBSInfo'
    { ebsOptimizedInfo = Lude.Nothing,
      encryptionSupport = Lude.Nothing,
      ebsOptimizedSupport = Lude.Nothing,
      nvmeSupport = Lude.Nothing
    }

-- | Describes the optimized EBS performance for the instance type.
--
-- /Note:/ Consider using 'ebsOptimizedInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiEBSOptimizedInfo :: Lens.Lens' EBSInfo (Lude.Maybe EBSOptimizedInfo)
eiEBSOptimizedInfo = Lens.lens (ebsOptimizedInfo :: EBSInfo -> Lude.Maybe EBSOptimizedInfo) (\s a -> s {ebsOptimizedInfo = a} :: EBSInfo)
{-# DEPRECATED eiEBSOptimizedInfo "Use generic-lens or generic-optics with 'ebsOptimizedInfo' instead." #-}

-- | Indicates whether Amazon EBS encryption is supported.
--
-- /Note:/ Consider using 'encryptionSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiEncryptionSupport :: Lens.Lens' EBSInfo (Lude.Maybe EBSEncryptionSupport)
eiEncryptionSupport = Lens.lens (encryptionSupport :: EBSInfo -> Lude.Maybe EBSEncryptionSupport) (\s a -> s {encryptionSupport = a} :: EBSInfo)
{-# DEPRECATED eiEncryptionSupport "Use generic-lens or generic-optics with 'encryptionSupport' instead." #-}

-- | Indicates whether the instance type is Amazon EBS-optimized. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSOptimized.html Amazon EBS-Optimized Instances> in /Amazon EC2 User Guide for Linux Instances/ .
--
-- /Note:/ Consider using 'ebsOptimizedSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiEBSOptimizedSupport :: Lens.Lens' EBSInfo (Lude.Maybe EBSOptimizedSupport)
eiEBSOptimizedSupport = Lens.lens (ebsOptimizedSupport :: EBSInfo -> Lude.Maybe EBSOptimizedSupport) (\s a -> s {ebsOptimizedSupport = a} :: EBSInfo)
{-# DEPRECATED eiEBSOptimizedSupport "Use generic-lens or generic-optics with 'ebsOptimizedSupport' instead." #-}

-- | Indicates whether non-volatile memory express (NVMe) is supported.
--
-- /Note:/ Consider using 'nvmeSupport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eiNvmeSupport :: Lens.Lens' EBSInfo (Lude.Maybe EBSNvmeSupport)
eiNvmeSupport = Lens.lens (nvmeSupport :: EBSInfo -> Lude.Maybe EBSNvmeSupport) (\s a -> s {nvmeSupport = a} :: EBSInfo)
{-# DEPRECATED eiNvmeSupport "Use generic-lens or generic-optics with 'nvmeSupport' instead." #-}

instance Lude.FromXML EBSInfo where
  parseXML x =
    EBSInfo'
      Lude.<$> (x Lude..@? "ebsOptimizedInfo")
      Lude.<*> (x Lude..@? "encryptionSupport")
      Lude.<*> (x Lude..@? "ebsOptimizedSupport")
      Lude.<*> (x Lude..@? "nvmeSupport")
