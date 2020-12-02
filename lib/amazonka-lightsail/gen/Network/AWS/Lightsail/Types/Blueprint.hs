{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Blueprint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Blueprint where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.BlueprintType
import Network.AWS.Lightsail.Types.InstancePlatform
import Network.AWS.Prelude

-- | Describes a blueprint (a virtual private server image).
--
--
--
-- /See:/ 'blueprint' smart constructor.
data Blueprint = Blueprint'
  { _bVersionCode :: !(Maybe Text),
    _bPlatform :: !(Maybe InstancePlatform),
    _bGroup :: !(Maybe Text),
    _bMinPower :: !(Maybe Int),
    _bProductURL :: !(Maybe Text),
    _bLicenseURL :: !(Maybe Text),
    _bName :: !(Maybe Text),
    _bVersion :: !(Maybe Text),
    _bBlueprintId :: !(Maybe Text),
    _bType :: !(Maybe BlueprintType),
    _bIsActive :: !(Maybe Bool),
    _bDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Blueprint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bVersionCode' - The version code.
--
-- * 'bPlatform' - The operating system platform (either Linux/Unix-based or Windows Server-based) of the blueprint.
--
-- * 'bGroup' - The group name of the blueprint (e.g., @amazon-linux@ ).
--
-- * 'bMinPower' - The minimum bundle power required to run this blueprint. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500. @0@ indicates that the blueprint runs on all instance sizes.
--
-- * 'bProductURL' - The product URL to learn more about the image or blueprint.
--
-- * 'bLicenseURL' - The end-user license agreement URL for the image or blueprint.
--
-- * 'bName' - The friendly name of the blueprint (e.g., @Amazon Linux@ ).
--
-- * 'bVersion' - The version number of the operating system, application, or stack (e.g., @2016.03.0@ ).
--
-- * 'bBlueprintId' - The ID for the virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ).
--
-- * 'bType' - The type of the blueprint (e.g., @os@ or @app@ ).
--
-- * 'bIsActive' - A Boolean value indicating whether the blueprint is active. Inactive blueprints are listed to support customers with existing instances but are not necessarily available for launch of new instances. Blueprints are marked inactive when they become outdated due to operating system updates or new application releases.
--
-- * 'bDescription' - The description of the blueprint.
blueprint ::
  Blueprint
blueprint =
  Blueprint'
    { _bVersionCode = Nothing,
      _bPlatform = Nothing,
      _bGroup = Nothing,
      _bMinPower = Nothing,
      _bProductURL = Nothing,
      _bLicenseURL = Nothing,
      _bName = Nothing,
      _bVersion = Nothing,
      _bBlueprintId = Nothing,
      _bType = Nothing,
      _bIsActive = Nothing,
      _bDescription = Nothing
    }

-- | The version code.
bVersionCode :: Lens' Blueprint (Maybe Text)
bVersionCode = lens _bVersionCode (\s a -> s {_bVersionCode = a})

-- | The operating system platform (either Linux/Unix-based or Windows Server-based) of the blueprint.
bPlatform :: Lens' Blueprint (Maybe InstancePlatform)
bPlatform = lens _bPlatform (\s a -> s {_bPlatform = a})

-- | The group name of the blueprint (e.g., @amazon-linux@ ).
bGroup :: Lens' Blueprint (Maybe Text)
bGroup = lens _bGroup (\s a -> s {_bGroup = a})

-- | The minimum bundle power required to run this blueprint. For example, you need a bundle with a power value of 500 or more to create an instance that uses a blueprint with a minimum power value of 500. @0@ indicates that the blueprint runs on all instance sizes.
bMinPower :: Lens' Blueprint (Maybe Int)
bMinPower = lens _bMinPower (\s a -> s {_bMinPower = a})

-- | The product URL to learn more about the image or blueprint.
bProductURL :: Lens' Blueprint (Maybe Text)
bProductURL = lens _bProductURL (\s a -> s {_bProductURL = a})

-- | The end-user license agreement URL for the image or blueprint.
bLicenseURL :: Lens' Blueprint (Maybe Text)
bLicenseURL = lens _bLicenseURL (\s a -> s {_bLicenseURL = a})

-- | The friendly name of the blueprint (e.g., @Amazon Linux@ ).
bName :: Lens' Blueprint (Maybe Text)
bName = lens _bName (\s a -> s {_bName = a})

-- | The version number of the operating system, application, or stack (e.g., @2016.03.0@ ).
bVersion :: Lens' Blueprint (Maybe Text)
bVersion = lens _bVersion (\s a -> s {_bVersion = a})

-- | The ID for the virtual private server image (e.g., @app_wordpress_4_4@ or @app_lamp_7_0@ ).
bBlueprintId :: Lens' Blueprint (Maybe Text)
bBlueprintId = lens _bBlueprintId (\s a -> s {_bBlueprintId = a})

-- | The type of the blueprint (e.g., @os@ or @app@ ).
bType :: Lens' Blueprint (Maybe BlueprintType)
bType = lens _bType (\s a -> s {_bType = a})

-- | A Boolean value indicating whether the blueprint is active. Inactive blueprints are listed to support customers with existing instances but are not necessarily available for launch of new instances. Blueprints are marked inactive when they become outdated due to operating system updates or new application releases.
bIsActive :: Lens' Blueprint (Maybe Bool)
bIsActive = lens _bIsActive (\s a -> s {_bIsActive = a})

-- | The description of the blueprint.
bDescription :: Lens' Blueprint (Maybe Text)
bDescription = lens _bDescription (\s a -> s {_bDescription = a})

instance FromJSON Blueprint where
  parseJSON =
    withObject
      "Blueprint"
      ( \x ->
          Blueprint'
            <$> (x .:? "versionCode")
            <*> (x .:? "platform")
            <*> (x .:? "group")
            <*> (x .:? "minPower")
            <*> (x .:? "productUrl")
            <*> (x .:? "licenseUrl")
            <*> (x .:? "name")
            <*> (x .:? "version")
            <*> (x .:? "blueprintId")
            <*> (x .:? "type")
            <*> (x .:? "isActive")
            <*> (x .:? "description")
      )

instance Hashable Blueprint

instance NFData Blueprint
