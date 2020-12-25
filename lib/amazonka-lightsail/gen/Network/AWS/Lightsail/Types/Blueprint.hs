{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Blueprint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Blueprint
  ( Blueprint (..),

    -- * Smart constructor
    mkBlueprint,

    -- * Lenses
    bBlueprintId,
    bDescription,
    bGroup,
    bIsActive,
    bLicenseUrl,
    bMinPower,
    bName,
    bPlatform,
    bProductUrl,
    bType,
    bVersion,
    bVersionCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.BlueprintId as Types
import qualified Network.AWS.Lightsail.Types.BlueprintType as Types
import qualified Network.AWS.Lightsail.Types.Description as Types
import qualified Network.AWS.Lightsail.Types.Group as Types
import qualified Network.AWS.Lightsail.Types.InstancePlatform as Types
import qualified Network.AWS.Lightsail.Types.LicenseUrl as Types
import qualified Network.AWS.Lightsail.Types.Name as Types
import qualified Network.AWS.Lightsail.Types.ProductUrl as Types
import qualified Network.AWS.Lightsail.Types.Version as Types
import qualified Network.AWS.Lightsail.Types.VersionCode as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a blueprint (a virtual private server image).
--
-- /See:/ 'mkBlueprint' smart constructor.
data Blueprint = Blueprint'
  { -- | The ID for the virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ).
    blueprintId :: Core.Maybe Types.BlueprintId,
    -- | The description of the blueprint.
    description :: Core.Maybe Types.Description,
    -- | The group name of the blueprint (e.g., @amazon-linux@ ).
    group :: Core.Maybe Types.Group,
    -- | A Boolean value indicating whether the blueprint is active. Inactive blueprints are listed to support customers with existing instances but are not necessarily available for launch of new instances. Blueprints are marked inactive when they become outdated due to operating system updates or new application releases.
    isActive :: Core.Maybe Core.Bool,
    -- | The end-user license agreement URL for the image or blueprint.
    licenseUrl :: Core.Maybe Types.LicenseUrl,
    -- | The minimum bundle power required to run this blueprint. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500. @0@ indicates that the blueprint runs on all instance sizes.
    minPower :: Core.Maybe Core.Int,
    -- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
    name :: Core.Maybe Types.Name,
    -- | The operating system platform (either Linux/Unix-based or Windows Server-based) of the blueprint.
    platform :: Core.Maybe Types.InstancePlatform,
    -- | The product URL to learn more about the image or blueprint.
    productUrl :: Core.Maybe Types.ProductUrl,
    -- | The type of the blueprint (e.g., @os@ or @app@ ).
    type' :: Core.Maybe Types.BlueprintType,
    -- | The version number of the operating system, application, or stack (e.g., @2016.03.0@ ).
    version :: Core.Maybe Types.Version,
    -- | The version code.
    versionCode :: Core.Maybe Types.VersionCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Blueprint' value with any optional fields omitted.
mkBlueprint ::
  Blueprint
mkBlueprint =
  Blueprint'
    { blueprintId = Core.Nothing,
      description = Core.Nothing,
      group = Core.Nothing,
      isActive = Core.Nothing,
      licenseUrl = Core.Nothing,
      minPower = Core.Nothing,
      name = Core.Nothing,
      platform = Core.Nothing,
      productUrl = Core.Nothing,
      type' = Core.Nothing,
      version = Core.Nothing,
      versionCode = Core.Nothing
    }

-- | The ID for the virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ).
--
-- /Note:/ Consider using 'blueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBlueprintId :: Lens.Lens' Blueprint (Core.Maybe Types.BlueprintId)
bBlueprintId = Lens.field @"blueprintId"
{-# DEPRECATED bBlueprintId "Use generic-lens or generic-optics with 'blueprintId' instead." #-}

-- | The description of the blueprint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDescription :: Lens.Lens' Blueprint (Core.Maybe Types.Description)
bDescription = Lens.field @"description"
{-# DEPRECATED bDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The group name of the blueprint (e.g., @amazon-linux@ ).
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bGroup :: Lens.Lens' Blueprint (Core.Maybe Types.Group)
bGroup = Lens.field @"group"
{-# DEPRECATED bGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | A Boolean value indicating whether the blueprint is active. Inactive blueprints are listed to support customers with existing instances but are not necessarily available for launch of new instances. Blueprints are marked inactive when they become outdated due to operating system updates or new application releases.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bIsActive :: Lens.Lens' Blueprint (Core.Maybe Core.Bool)
bIsActive = Lens.field @"isActive"
{-# DEPRECATED bIsActive "Use generic-lens or generic-optics with 'isActive' instead." #-}

-- | The end-user license agreement URL for the image or blueprint.
--
-- /Note:/ Consider using 'licenseUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bLicenseUrl :: Lens.Lens' Blueprint (Core.Maybe Types.LicenseUrl)
bLicenseUrl = Lens.field @"licenseUrl"
{-# DEPRECATED bLicenseUrl "Use generic-lens or generic-optics with 'licenseUrl' instead." #-}

-- | The minimum bundle power required to run this blueprint. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500. @0@ indicates that the blueprint runs on all instance sizes.
--
-- /Note:/ Consider using 'minPower' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bMinPower :: Lens.Lens' Blueprint (Core.Maybe Core.Int)
bMinPower = Lens.field @"minPower"
{-# DEPRECATED bMinPower "Use generic-lens or generic-optics with 'minPower' instead." #-}

-- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bName :: Lens.Lens' Blueprint (Core.Maybe Types.Name)
bName = Lens.field @"name"
{-# DEPRECATED bName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The operating system platform (either Linux/Unix-based or Windows Server-based) of the blueprint.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPlatform :: Lens.Lens' Blueprint (Core.Maybe Types.InstancePlatform)
bPlatform = Lens.field @"platform"
{-# DEPRECATED bPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The product URL to learn more about the image or blueprint.
--
-- /Note:/ Consider using 'productUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bProductUrl :: Lens.Lens' Blueprint (Core.Maybe Types.ProductUrl)
bProductUrl = Lens.field @"productUrl"
{-# DEPRECATED bProductUrl "Use generic-lens or generic-optics with 'productUrl' instead." #-}

-- | The type of the blueprint (e.g., @os@ or @app@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bType :: Lens.Lens' Blueprint (Core.Maybe Types.BlueprintType)
bType = Lens.field @"type'"
{-# DEPRECATED bType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The version number of the operating system, application, or stack (e.g., @2016.03.0@ ).
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bVersion :: Lens.Lens' Blueprint (Core.Maybe Types.Version)
bVersion = Lens.field @"version"
{-# DEPRECATED bVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The version code.
--
-- /Note:/ Consider using 'versionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bVersionCode :: Lens.Lens' Blueprint (Core.Maybe Types.VersionCode)
bVersionCode = Lens.field @"versionCode"
{-# DEPRECATED bVersionCode "Use generic-lens or generic-optics with 'versionCode' instead." #-}

instance Core.FromJSON Blueprint where
  parseJSON =
    Core.withObject "Blueprint" Core.$
      \x ->
        Blueprint'
          Core.<$> (x Core..:? "blueprintId")
          Core.<*> (x Core..:? "description")
          Core.<*> (x Core..:? "group")
          Core.<*> (x Core..:? "isActive")
          Core.<*> (x Core..:? "licenseUrl")
          Core.<*> (x Core..:? "minPower")
          Core.<*> (x Core..:? "name")
          Core.<*> (x Core..:? "platform")
          Core.<*> (x Core..:? "productUrl")
          Core.<*> (x Core..:? "type")
          Core.<*> (x Core..:? "version")
          Core.<*> (x Core..:? "versionCode")
