{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.S3ReportExportConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.S3ReportExportConfig
  ( S3ReportExportConfig (..)
  -- * Smart constructor
  , mkS3ReportExportConfig
  -- * Lenses
  , srecBucket
  , srecEncryptionDisabled
  , srecEncryptionKey
  , srecPackaging
  , srecPath
  ) where

import qualified Network.AWS.CodeBuild.Types.Bucket as Types
import qualified Network.AWS.CodeBuild.Types.EncryptionKey as Types
import qualified Network.AWS.CodeBuild.Types.ReportPackagingType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the S3 bucket where the raw data of a report are exported. 
--
-- /See:/ 'mkS3ReportExportConfig' smart constructor.
data S3ReportExportConfig = S3ReportExportConfig'
  { bucket :: Core.Maybe Types.Bucket
    -- ^ The name of the S3 bucket where the raw data of a report are exported. 
  , encryptionDisabled :: Core.Maybe Core.Bool
    -- ^ A boolean value that specifies if the results of a report are encrypted. 
  , encryptionKey :: Core.Maybe Types.EncryptionKey
    -- ^ The encryption key for the report's encrypted raw data. 
  , packaging :: Core.Maybe Types.ReportPackagingType
    -- ^ The type of build output artifact to create. Valid values include: 
--
--
--     * @NONE@ : AWS CodeBuild creates the raw data in the output bucket. This is the default if packaging is not specified. 
--
--
--     * @ZIP@ : AWS CodeBuild creates a ZIP file with the raw data in the output bucket. 
--
--
  , path :: Core.Maybe Core.Text
    -- ^ The path to the exported report's raw data results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'S3ReportExportConfig' value with any optional fields omitted.
mkS3ReportExportConfig
    :: S3ReportExportConfig
mkS3ReportExportConfig
  = S3ReportExportConfig'{bucket = Core.Nothing,
                          encryptionDisabled = Core.Nothing, encryptionKey = Core.Nothing,
                          packaging = Core.Nothing, path = Core.Nothing}

-- | The name of the S3 bucket where the raw data of a report are exported. 
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srecBucket :: Lens.Lens' S3ReportExportConfig (Core.Maybe Types.Bucket)
srecBucket = Lens.field @"bucket"
{-# INLINEABLE srecBucket #-}
{-# DEPRECATED bucket "Use generic-lens or generic-optics with 'bucket' instead"  #-}

-- | A boolean value that specifies if the results of a report are encrypted. 
--
-- /Note:/ Consider using 'encryptionDisabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srecEncryptionDisabled :: Lens.Lens' S3ReportExportConfig (Core.Maybe Core.Bool)
srecEncryptionDisabled = Lens.field @"encryptionDisabled"
{-# INLINEABLE srecEncryptionDisabled #-}
{-# DEPRECATED encryptionDisabled "Use generic-lens or generic-optics with 'encryptionDisabled' instead"  #-}

-- | The encryption key for the report's encrypted raw data. 
--
-- /Note:/ Consider using 'encryptionKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srecEncryptionKey :: Lens.Lens' S3ReportExportConfig (Core.Maybe Types.EncryptionKey)
srecEncryptionKey = Lens.field @"encryptionKey"
{-# INLINEABLE srecEncryptionKey #-}
{-# DEPRECATED encryptionKey "Use generic-lens or generic-optics with 'encryptionKey' instead"  #-}

-- | The type of build output artifact to create. Valid values include: 
--
--
--     * @NONE@ : AWS CodeBuild creates the raw data in the output bucket. This is the default if packaging is not specified. 
--
--
--     * @ZIP@ : AWS CodeBuild creates a ZIP file with the raw data in the output bucket. 
--
--
--
-- /Note:/ Consider using 'packaging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srecPackaging :: Lens.Lens' S3ReportExportConfig (Core.Maybe Types.ReportPackagingType)
srecPackaging = Lens.field @"packaging"
{-# INLINEABLE srecPackaging #-}
{-# DEPRECATED packaging "Use generic-lens or generic-optics with 'packaging' instead"  #-}

-- | The path to the exported report's raw data results. 
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srecPath :: Lens.Lens' S3ReportExportConfig (Core.Maybe Core.Text)
srecPath = Lens.field @"path"
{-# INLINEABLE srecPath #-}
{-# DEPRECATED path "Use generic-lens or generic-optics with 'path' instead"  #-}

instance Core.FromJSON S3ReportExportConfig where
        toJSON S3ReportExportConfig{..}
          = Core.object
              (Core.catMaybes
                 [("bucket" Core..=) Core.<$> bucket,
                  ("encryptionDisabled" Core..=) Core.<$> encryptionDisabled,
                  ("encryptionKey" Core..=) Core.<$> encryptionKey,
                  ("packaging" Core..=) Core.<$> packaging,
                  ("path" Core..=) Core.<$> path])

instance Core.FromJSON S3ReportExportConfig where
        parseJSON
          = Core.withObject "S3ReportExportConfig" Core.$
              \ x ->
                S3ReportExportConfig' Core.<$>
                  (x Core..:? "bucket") Core.<*> x Core..:? "encryptionDisabled"
                    Core.<*> x Core..:? "encryptionKey"
                    Core.<*> x Core..:? "packaging"
                    Core.<*> x Core..:? "path"
