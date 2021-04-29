{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Blueprint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Blueprint where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.BlueprintType
import Network.AWS.Lightsail.Types.InstancePlatform
import qualified Network.AWS.Prelude as Prelude

-- | Describes a blueprint (a virtual private server image).
--
-- /See:/ 'newBlueprint' smart constructor.
data Blueprint = Blueprint'
  { -- | The operating system platform (either Linux\/Unix-based or Windows
    -- Server-based) of the blueprint.
    platform :: Prelude.Maybe InstancePlatform,
    -- | A Boolean value indicating whether the blueprint is active. Inactive
    -- blueprints are listed to support customers with existing instances but
    -- are not necessarily available for launch of new instances. Blueprints
    -- are marked inactive when they become outdated due to operating system
    -- updates or new application releases.
    isActive :: Prelude.Maybe Prelude.Bool,
    -- | The end-user license agreement URL for the image or blueprint.
    licenseUrl :: Prelude.Maybe Prelude.Text,
    -- | The product URL to learn more about the image or blueprint.
    productUrl :: Prelude.Maybe Prelude.Text,
    -- | The version number of the operating system, application, or stack (e.g.,
    -- @2016.03.0@).
    version :: Prelude.Maybe Prelude.Text,
    -- | The ID for the virtual private server image (e.g., @app_wordpress_4_4@
    -- or @app_lamp_7_0@).
    blueprintId :: Prelude.Maybe Prelude.Text,
    -- | The version code.
    versionCode :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the blueprint (e.g., @Amazon Linux@).
    name :: Prelude.Maybe Prelude.Text,
    -- | The group name of the blueprint (e.g., @amazon-linux@).
    group' :: Prelude.Maybe Prelude.Text,
    -- | The description of the blueprint.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of the blueprint (e.g., @os@ or @app@).
    type' :: Prelude.Maybe BlueprintType,
    -- | The minimum bundle power required to run this blueprint. For example,
    -- you need a bundle with a power value of 500 or more to create an
    -- instance that uses a blueprint with a minimum power value of 500. @0@
    -- indicates that the blueprint runs on all instance sizes.
    minPower :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Blueprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platform', 'blueprint_platform' - The operating system platform (either Linux\/Unix-based or Windows
-- Server-based) of the blueprint.
--
-- 'isActive', 'blueprint_isActive' - A Boolean value indicating whether the blueprint is active. Inactive
-- blueprints are listed to support customers with existing instances but
-- are not necessarily available for launch of new instances. Blueprints
-- are marked inactive when they become outdated due to operating system
-- updates or new application releases.
--
-- 'licenseUrl', 'blueprint_licenseUrl' - The end-user license agreement URL for the image or blueprint.
--
-- 'productUrl', 'blueprint_productUrl' - The product URL to learn more about the image or blueprint.
--
-- 'version', 'blueprint_version' - The version number of the operating system, application, or stack (e.g.,
-- @2016.03.0@).
--
-- 'blueprintId', 'blueprint_blueprintId' - The ID for the virtual private server image (e.g., @app_wordpress_4_4@
-- or @app_lamp_7_0@).
--
-- 'versionCode', 'blueprint_versionCode' - The version code.
--
-- 'name', 'blueprint_name' - The friendly name of the blueprint (e.g., @Amazon Linux@).
--
-- 'group'', 'blueprint_group' - The group name of the blueprint (e.g., @amazon-linux@).
--
-- 'description', 'blueprint_description' - The description of the blueprint.
--
-- 'type'', 'blueprint_type' - The type of the blueprint (e.g., @os@ or @app@).
--
-- 'minPower', 'blueprint_minPower' - The minimum bundle power required to run this blueprint. For example,
-- you need a bundle with a power value of 500 or more to create an
-- instance that uses a blueprint with a minimum power value of 500. @0@
-- indicates that the blueprint runs on all instance sizes.
newBlueprint ::
  Blueprint
newBlueprint =
  Blueprint'
    { platform = Prelude.Nothing,
      isActive = Prelude.Nothing,
      licenseUrl = Prelude.Nothing,
      productUrl = Prelude.Nothing,
      version = Prelude.Nothing,
      blueprintId = Prelude.Nothing,
      versionCode = Prelude.Nothing,
      name = Prelude.Nothing,
      group' = Prelude.Nothing,
      description = Prelude.Nothing,
      type' = Prelude.Nothing,
      minPower = Prelude.Nothing
    }

-- | The operating system platform (either Linux\/Unix-based or Windows
-- Server-based) of the blueprint.
blueprint_platform :: Lens.Lens' Blueprint (Prelude.Maybe InstancePlatform)
blueprint_platform = Lens.lens (\Blueprint' {platform} -> platform) (\s@Blueprint' {} a -> s {platform = a} :: Blueprint)

-- | A Boolean value indicating whether the blueprint is active. Inactive
-- blueprints are listed to support customers with existing instances but
-- are not necessarily available for launch of new instances. Blueprints
-- are marked inactive when they become outdated due to operating system
-- updates or new application releases.
blueprint_isActive :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Bool)
blueprint_isActive = Lens.lens (\Blueprint' {isActive} -> isActive) (\s@Blueprint' {} a -> s {isActive = a} :: Blueprint)

-- | The end-user license agreement URL for the image or blueprint.
blueprint_licenseUrl :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_licenseUrl = Lens.lens (\Blueprint' {licenseUrl} -> licenseUrl) (\s@Blueprint' {} a -> s {licenseUrl = a} :: Blueprint)

-- | The product URL to learn more about the image or blueprint.
blueprint_productUrl :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_productUrl = Lens.lens (\Blueprint' {productUrl} -> productUrl) (\s@Blueprint' {} a -> s {productUrl = a} :: Blueprint)

-- | The version number of the operating system, application, or stack (e.g.,
-- @2016.03.0@).
blueprint_version :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_version = Lens.lens (\Blueprint' {version} -> version) (\s@Blueprint' {} a -> s {version = a} :: Blueprint)

-- | The ID for the virtual private server image (e.g., @app_wordpress_4_4@
-- or @app_lamp_7_0@).
blueprint_blueprintId :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_blueprintId = Lens.lens (\Blueprint' {blueprintId} -> blueprintId) (\s@Blueprint' {} a -> s {blueprintId = a} :: Blueprint)

-- | The version code.
blueprint_versionCode :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_versionCode = Lens.lens (\Blueprint' {versionCode} -> versionCode) (\s@Blueprint' {} a -> s {versionCode = a} :: Blueprint)

-- | The friendly name of the blueprint (e.g., @Amazon Linux@).
blueprint_name :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_name = Lens.lens (\Blueprint' {name} -> name) (\s@Blueprint' {} a -> s {name = a} :: Blueprint)

-- | The group name of the blueprint (e.g., @amazon-linux@).
blueprint_group :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_group = Lens.lens (\Blueprint' {group'} -> group') (\s@Blueprint' {} a -> s {group' = a} :: Blueprint)

-- | The description of the blueprint.
blueprint_description :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Text)
blueprint_description = Lens.lens (\Blueprint' {description} -> description) (\s@Blueprint' {} a -> s {description = a} :: Blueprint)

-- | The type of the blueprint (e.g., @os@ or @app@).
blueprint_type :: Lens.Lens' Blueprint (Prelude.Maybe BlueprintType)
blueprint_type = Lens.lens (\Blueprint' {type'} -> type') (\s@Blueprint' {} a -> s {type' = a} :: Blueprint)

-- | The minimum bundle power required to run this blueprint. For example,
-- you need a bundle with a power value of 500 or more to create an
-- instance that uses a blueprint with a minimum power value of 500. @0@
-- indicates that the blueprint runs on all instance sizes.
blueprint_minPower :: Lens.Lens' Blueprint (Prelude.Maybe Prelude.Int)
blueprint_minPower = Lens.lens (\Blueprint' {minPower} -> minPower) (\s@Blueprint' {} a -> s {minPower = a} :: Blueprint)

instance Prelude.FromJSON Blueprint where
  parseJSON =
    Prelude.withObject
      "Blueprint"
      ( \x ->
          Blueprint'
            Prelude.<$> (x Prelude..:? "platform")
            Prelude.<*> (x Prelude..:? "isActive")
            Prelude.<*> (x Prelude..:? "licenseUrl")
            Prelude.<*> (x Prelude..:? "productUrl")
            Prelude.<*> (x Prelude..:? "version")
            Prelude.<*> (x Prelude..:? "blueprintId")
            Prelude.<*> (x Prelude..:? "versionCode")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "group")
            Prelude.<*> (x Prelude..:? "description")
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..:? "minPower")
      )

instance Prelude.Hashable Blueprint

instance Prelude.NFData Blueprint
