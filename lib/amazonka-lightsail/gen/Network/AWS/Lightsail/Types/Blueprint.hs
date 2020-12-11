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
    bVersionCode,
    bPlatform,
    bGroup,
    bMinPower,
    bProductURL,
    bLicenseURL,
    bName,
    bVersion,
    bBlueprintId,
    bType,
    bIsActive,
    bDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.BlueprintType
import Network.AWS.Lightsail.Types.InstancePlatform
import qualified Network.AWS.Prelude as Lude

-- | Describes a blueprint (a virtual private server image).
--
-- /See:/ 'mkBlueprint' smart constructor.
data Blueprint = Blueprint'
  { versionCode :: Lude.Maybe Lude.Text,
    platform :: Lude.Maybe InstancePlatform,
    group :: Lude.Maybe Lude.Text,
    minPower :: Lude.Maybe Lude.Int,
    productURL :: Lude.Maybe Lude.Text,
    licenseURL :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Lude.Text,
    blueprintId :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe BlueprintType,
    isActive :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Blueprint' with the minimum fields required to make a request.
--
-- * 'blueprintId' - The ID for the virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ).
-- * 'description' - The description of the blueprint.
-- * 'group' - The group name of the blueprint (e.g., @amazon-linux@ ).
-- * 'isActive' - A Boolean value indicating whether the blueprint is active. Inactive blueprints are listed to support customers with existing instances but are not necessarily available for launch of new instances. Blueprints are marked inactive when they become outdated due to operating system updates or new application releases.
-- * 'licenseURL' - The end-user license agreement URL for the image or blueprint.
-- * 'minPower' - The minimum bundle power required to run this blueprint. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500. @0@ indicates that the blueprint runs on all instance sizes.
-- * 'name' - The friendly name of the blueprint (e.g., @Amazon Linux@ ).
-- * 'platform' - The operating system platform (either Linux/Unix-based or Windows Server-based) of the blueprint.
-- * 'productURL' - The product URL to learn more about the image or blueprint.
-- * 'type'' - The type of the blueprint (e.g., @os@ or @app@ ).
-- * 'version' - The version number of the operating system, application, or stack (e.g., @2016.03.0@ ).
-- * 'versionCode' - The version code.
mkBlueprint ::
  Blueprint
mkBlueprint =
  Blueprint'
    { versionCode = Lude.Nothing,
      platform = Lude.Nothing,
      group = Lude.Nothing,
      minPower = Lude.Nothing,
      productURL = Lude.Nothing,
      licenseURL = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      blueprintId = Lude.Nothing,
      type' = Lude.Nothing,
      isActive = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The version code.
--
-- /Note:/ Consider using 'versionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bVersionCode :: Lens.Lens' Blueprint (Lude.Maybe Lude.Text)
bVersionCode = Lens.lens (versionCode :: Blueprint -> Lude.Maybe Lude.Text) (\s a -> s {versionCode = a} :: Blueprint)
{-# DEPRECATED bVersionCode "Use generic-lens or generic-optics with 'versionCode' instead." #-}

-- | The operating system platform (either Linux/Unix-based or Windows Server-based) of the blueprint.
--
-- /Note:/ Consider using 'platform' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bPlatform :: Lens.Lens' Blueprint (Lude.Maybe InstancePlatform)
bPlatform = Lens.lens (platform :: Blueprint -> Lude.Maybe InstancePlatform) (\s a -> s {platform = a} :: Blueprint)
{-# DEPRECATED bPlatform "Use generic-lens or generic-optics with 'platform' instead." #-}

-- | The group name of the blueprint (e.g., @amazon-linux@ ).
--
-- /Note:/ Consider using 'group' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bGroup :: Lens.Lens' Blueprint (Lude.Maybe Lude.Text)
bGroup = Lens.lens (group :: Blueprint -> Lude.Maybe Lude.Text) (\s a -> s {group = a} :: Blueprint)
{-# DEPRECATED bGroup "Use generic-lens or generic-optics with 'group' instead." #-}

-- | The minimum bundle power required to run this blueprint. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500. @0@ indicates that the blueprint runs on all instance sizes.
--
-- /Note:/ Consider using 'minPower' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bMinPower :: Lens.Lens' Blueprint (Lude.Maybe Lude.Int)
bMinPower = Lens.lens (minPower :: Blueprint -> Lude.Maybe Lude.Int) (\s a -> s {minPower = a} :: Blueprint)
{-# DEPRECATED bMinPower "Use generic-lens or generic-optics with 'minPower' instead." #-}

-- | The product URL to learn more about the image or blueprint.
--
-- /Note:/ Consider using 'productURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bProductURL :: Lens.Lens' Blueprint (Lude.Maybe Lude.Text)
bProductURL = Lens.lens (productURL :: Blueprint -> Lude.Maybe Lude.Text) (\s a -> s {productURL = a} :: Blueprint)
{-# DEPRECATED bProductURL "Use generic-lens or generic-optics with 'productURL' instead." #-}

-- | The end-user license agreement URL for the image or blueprint.
--
-- /Note:/ Consider using 'licenseURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bLicenseURL :: Lens.Lens' Blueprint (Lude.Maybe Lude.Text)
bLicenseURL = Lens.lens (licenseURL :: Blueprint -> Lude.Maybe Lude.Text) (\s a -> s {licenseURL = a} :: Blueprint)
{-# DEPRECATED bLicenseURL "Use generic-lens or generic-optics with 'licenseURL' instead." #-}

-- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bName :: Lens.Lens' Blueprint (Lude.Maybe Lude.Text)
bName = Lens.lens (name :: Blueprint -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Blueprint)
{-# DEPRECATED bName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version number of the operating system, application, or stack (e.g., @2016.03.0@ ).
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bVersion :: Lens.Lens' Blueprint (Lude.Maybe Lude.Text)
bVersion = Lens.lens (version :: Blueprint -> Lude.Maybe Lude.Text) (\s a -> s {version = a} :: Blueprint)
{-# DEPRECATED bVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The ID for the virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ).
--
-- /Note:/ Consider using 'blueprintId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bBlueprintId :: Lens.Lens' Blueprint (Lude.Maybe Lude.Text)
bBlueprintId = Lens.lens (blueprintId :: Blueprint -> Lude.Maybe Lude.Text) (\s a -> s {blueprintId = a} :: Blueprint)
{-# DEPRECATED bBlueprintId "Use generic-lens or generic-optics with 'blueprintId' instead." #-}

-- | The type of the blueprint (e.g., @os@ or @app@ ).
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bType :: Lens.Lens' Blueprint (Lude.Maybe BlueprintType)
bType = Lens.lens (type' :: Blueprint -> Lude.Maybe BlueprintType) (\s a -> s {type' = a} :: Blueprint)
{-# DEPRECATED bType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A Boolean value indicating whether the blueprint is active. Inactive blueprints are listed to support customers with existing instances but are not necessarily available for launch of new instances. Blueprints are marked inactive when they become outdated due to operating system updates or new application releases.
--
-- /Note:/ Consider using 'isActive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bIsActive :: Lens.Lens' Blueprint (Lude.Maybe Lude.Bool)
bIsActive = Lens.lens (isActive :: Blueprint -> Lude.Maybe Lude.Bool) (\s a -> s {isActive = a} :: Blueprint)
{-# DEPRECATED bIsActive "Use generic-lens or generic-optics with 'isActive' instead." #-}

-- | The description of the blueprint.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bDescription :: Lens.Lens' Blueprint (Lude.Maybe Lude.Text)
bDescription = Lens.lens (description :: Blueprint -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Blueprint)
{-# DEPRECATED bDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Blueprint where
  parseJSON =
    Lude.withObject
      "Blueprint"
      ( \x ->
          Blueprint'
            Lude.<$> (x Lude..:? "versionCode")
            Lude.<*> (x Lude..:? "platform")
            Lude.<*> (x Lude..:? "group")
            Lude.<*> (x Lude..:? "minPower")
            Lude.<*> (x Lude..:? "productUrl")
            Lude.<*> (x Lude..:? "licenseUrl")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "version")
            Lude.<*> (x Lude..:? "blueprintId")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "isActive")
            Lude.<*> (x Lude..:? "description")
      )
