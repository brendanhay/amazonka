{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.GetLayerVersionResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.GetLayerVersionResponse
  ( GetLayerVersionResponse (..)
  -- * Smart constructor
  , mkGetLayerVersionResponse
  -- * Lenses
  , glvrCompatibleRuntimes
  , glvrContent
  , glvrCreatedDate
  , glvrDescription
  , glvrLayerArn
  , glvrLayerVersionArn
  , glvrLicenseInfo
  , glvrVersion
  ) where

import qualified Network.AWS.Lambda.Types.CreatedDate as Types
import qualified Network.AWS.Lambda.Types.Description as Types
import qualified Network.AWS.Lambda.Types.LayerArn as Types
import qualified Network.AWS.Lambda.Types.LayerVersionArn as Types
import qualified Network.AWS.Lambda.Types.LayerVersionContentOutput as Types
import qualified Network.AWS.Lambda.Types.LicenseInfo as Types
import qualified Network.AWS.Lambda.Types.Runtime as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | /See:/ 'mkGetLayerVersionResponse' smart constructor.
data GetLayerVersionResponse = GetLayerVersionResponse'
  { compatibleRuntimes :: Core.Maybe [Types.Runtime]
    -- ^ The layer's compatible runtimes.
  , content :: Core.Maybe Types.LayerVersionContentOutput
    -- ^ Details about the layer version.
  , createdDate :: Core.Maybe Types.CreatedDate
    -- ^ The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
  , description :: Core.Maybe Types.Description
    -- ^ The description of the version.
  , layerArn :: Core.Maybe Types.LayerArn
    -- ^ The ARN of the layer.
  , layerVersionArn :: Core.Maybe Types.LayerVersionArn
    -- ^ The ARN of the layer version.
  , licenseInfo :: Core.Maybe Types.LicenseInfo
    -- ^ The layer's software license.
  , version :: Core.Maybe Core.Integer
    -- ^ The version number.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetLayerVersionResponse' value with any optional fields omitted.
mkGetLayerVersionResponse
    :: GetLayerVersionResponse
mkGetLayerVersionResponse
  = GetLayerVersionResponse'{compatibleRuntimes = Core.Nothing,
                             content = Core.Nothing, createdDate = Core.Nothing,
                             description = Core.Nothing, layerArn = Core.Nothing,
                             layerVersionArn = Core.Nothing, licenseInfo = Core.Nothing,
                             version = Core.Nothing}

-- | The layer's compatible runtimes.
--
-- /Note:/ Consider using 'compatibleRuntimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvrCompatibleRuntimes :: Lens.Lens' GetLayerVersionResponse (Core.Maybe [Types.Runtime])
glvrCompatibleRuntimes = Lens.field @"compatibleRuntimes"
{-# INLINEABLE glvrCompatibleRuntimes #-}
{-# DEPRECATED compatibleRuntimes "Use generic-lens or generic-optics with 'compatibleRuntimes' instead"  #-}

-- | Details about the layer version.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvrContent :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Types.LayerVersionContentOutput)
glvrContent = Lens.field @"content"
{-# INLINEABLE glvrContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

-- | The date that the layer version was created, in <https://www.w3.org/TR/NOTE-datetime ISO-8601 format> (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvrCreatedDate :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Types.CreatedDate)
glvrCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE glvrCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The description of the version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvrDescription :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Types.Description)
glvrDescription = Lens.field @"description"
{-# INLINEABLE glvrDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ARN of the layer.
--
-- /Note:/ Consider using 'layerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvrLayerArn :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Types.LayerArn)
glvrLayerArn = Lens.field @"layerArn"
{-# INLINEABLE glvrLayerArn #-}
{-# DEPRECATED layerArn "Use generic-lens or generic-optics with 'layerArn' instead"  #-}

-- | The ARN of the layer version.
--
-- /Note:/ Consider using 'layerVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvrLayerVersionArn :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Types.LayerVersionArn)
glvrLayerVersionArn = Lens.field @"layerVersionArn"
{-# INLINEABLE glvrLayerVersionArn #-}
{-# DEPRECATED layerVersionArn "Use generic-lens or generic-optics with 'layerVersionArn' instead"  #-}

-- | The layer's software license.
--
-- /Note:/ Consider using 'licenseInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvrLicenseInfo :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Types.LicenseInfo)
glvrLicenseInfo = Lens.field @"licenseInfo"
{-# INLINEABLE glvrLicenseInfo #-}
{-# DEPRECATED licenseInfo "Use generic-lens or generic-optics with 'licenseInfo' instead"  #-}

-- | The version number.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvrVersion :: Lens.Lens' GetLayerVersionResponse (Core.Maybe Core.Integer)
glvrVersion = Lens.field @"version"
{-# INLINEABLE glvrVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON GetLayerVersionResponse where
        parseJSON
          = Core.withObject "GetLayerVersionResponse" Core.$
              \ x ->
                GetLayerVersionResponse' Core.<$>
                  (x Core..:? "CompatibleRuntimes") Core.<*> x Core..:? "Content"
                    Core.<*> x Core..:? "CreatedDate"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "LayerArn"
                    Core.<*> x Core..:? "LayerVersionArn"
                    Core.<*> x Core..:? "LicenseInfo"
                    Core.<*> x Core..:? "Version"
