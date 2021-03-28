{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.LayerVersionsListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.LayerVersionsListItem
  ( LayerVersionsListItem (..)
  -- * Smart constructor
  , mkLayerVersionsListItem
  -- * Lenses
  , lvliCompatibleRuntimes
  , lvliCreatedDate
  , lvliDescription
  , lvliLayerVersionArn
  , lvliLicenseInfo
  , lvliVersion
  ) where

import qualified Network.AWS.Lambda.Types.CreatedDate as Types
import qualified Network.AWS.Lambda.Types.Description as Types
import qualified Network.AWS.Lambda.Types.LayerVersionArn as Types
import qualified Network.AWS.Lambda.Types.LicenseInfo as Types
import qualified Network.AWS.Lambda.Types.Runtime as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> .
--
-- /See:/ 'mkLayerVersionsListItem' smart constructor.
data LayerVersionsListItem = LayerVersionsListItem'
  { compatibleRuntimes :: Core.Maybe [Types.Runtime]
    -- ^ The layer's compatible runtimes.
  , createdDate :: Core.Maybe Types.CreatedDate
    -- ^ The date that the version was created, in ISO 8601 format. For example, @2018-11-27T15:10:45.123+0000@ .
  , description :: Core.Maybe Types.Description
    -- ^ The description of the version.
  , layerVersionArn :: Core.Maybe Types.LayerVersionArn
    -- ^ The ARN of the layer version.
  , licenseInfo :: Core.Maybe Types.LicenseInfo
    -- ^ The layer's open-source license.
  , version :: Core.Maybe Core.Integer
    -- ^ The version number.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LayerVersionsListItem' value with any optional fields omitted.
mkLayerVersionsListItem
    :: LayerVersionsListItem
mkLayerVersionsListItem
  = LayerVersionsListItem'{compatibleRuntimes = Core.Nothing,
                           createdDate = Core.Nothing, description = Core.Nothing,
                           layerVersionArn = Core.Nothing, licenseInfo = Core.Nothing,
                           version = Core.Nothing}

-- | The layer's compatible runtimes.
--
-- /Note:/ Consider using 'compatibleRuntimes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliCompatibleRuntimes :: Lens.Lens' LayerVersionsListItem (Core.Maybe [Types.Runtime])
lvliCompatibleRuntimes = Lens.field @"compatibleRuntimes"
{-# INLINEABLE lvliCompatibleRuntimes #-}
{-# DEPRECATED compatibleRuntimes "Use generic-lens or generic-optics with 'compatibleRuntimes' instead"  #-}

-- | The date that the version was created, in ISO 8601 format. For example, @2018-11-27T15:10:45.123+0000@ .
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliCreatedDate :: Lens.Lens' LayerVersionsListItem (Core.Maybe Types.CreatedDate)
lvliCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE lvliCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | The description of the version.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliDescription :: Lens.Lens' LayerVersionsListItem (Core.Maybe Types.Description)
lvliDescription = Lens.field @"description"
{-# INLINEABLE lvliDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The ARN of the layer version.
--
-- /Note:/ Consider using 'layerVersionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliLayerVersionArn :: Lens.Lens' LayerVersionsListItem (Core.Maybe Types.LayerVersionArn)
lvliLayerVersionArn = Lens.field @"layerVersionArn"
{-# INLINEABLE lvliLayerVersionArn #-}
{-# DEPRECATED layerVersionArn "Use generic-lens or generic-optics with 'layerVersionArn' instead"  #-}

-- | The layer's open-source license.
--
-- /Note:/ Consider using 'licenseInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliLicenseInfo :: Lens.Lens' LayerVersionsListItem (Core.Maybe Types.LicenseInfo)
lvliLicenseInfo = Lens.field @"licenseInfo"
{-# INLINEABLE lvliLicenseInfo #-}
{-# DEPRECATED licenseInfo "Use generic-lens or generic-optics with 'licenseInfo' instead"  #-}

-- | The version number.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvliVersion :: Lens.Lens' LayerVersionsListItem (Core.Maybe Core.Integer)
lvliVersion = Lens.field @"version"
{-# INLINEABLE lvliVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

instance Core.FromJSON LayerVersionsListItem where
        parseJSON
          = Core.withObject "LayerVersionsListItem" Core.$
              \ x ->
                LayerVersionsListItem' Core.<$>
                  (x Core..:? "CompatibleRuntimes") Core.<*> x Core..:? "CreatedDate"
                    Core.<*> x Core..:? "Description"
                    Core.<*> x Core..:? "LayerVersionArn"
                    Core.<*> x Core..:? "LicenseInfo"
                    Core.<*> x Core..:? "Version"
