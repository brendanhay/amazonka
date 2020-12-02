{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroupOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionGroupOption where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.OptionGroupOptionSetting
import Network.AWS.RDS.Types.OptionVersion

-- | Available option.
--
--
--
-- /See:/ 'optionGroupOption' smart constructor.
data OptionGroupOption = OptionGroupOption'
  { _ogoMinimumRequiredMinorEngineVersion ::
      !(Maybe Text),
    _ogoOptionsConflictsWith :: !(Maybe [Text]),
    _ogoPermanent :: !(Maybe Bool),
    _ogoPersistent :: !(Maybe Bool),
    _ogoOptionGroupOptionVersions ::
      !(Maybe [OptionVersion]),
    _ogoEngineName :: !(Maybe Text),
    _ogoMajorEngineVersion :: !(Maybe Text),
    _ogoName :: !(Maybe Text),
    _ogoSupportsOptionVersionDowngrade :: !(Maybe Bool),
    _ogoDefaultPort :: !(Maybe Int),
    _ogoOptionGroupOptionSettings ::
      !(Maybe [OptionGroupOptionSetting]),
    _ogoRequiresAutoMinorEngineVersionUpgrade ::
      !(Maybe Bool),
    _ogoPortRequired :: !(Maybe Bool),
    _ogoDescription :: !(Maybe Text),
    _ogoOptionsDependedOn :: !(Maybe [Text]),
    _ogoVPCOnly :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OptionGroupOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ogoMinimumRequiredMinorEngineVersion' - The minimum required engine version for the option to be applied.
--
-- * 'ogoOptionsConflictsWith' - The options that conflict with this option.
--
-- * 'ogoPermanent' - Permanent options can never be removed from an option group. An option group containing a permanent option can't be removed from a DB instance.
--
-- * 'ogoPersistent' - Persistent options can't be removed from an option group while DB instances are associated with the option group. If you disassociate all DB instances from the option group, your can remove the persistent option from the option group.
--
-- * 'ogoOptionGroupOptionVersions' - The versions that are available for the option.
--
-- * 'ogoEngineName' - The name of the engine that this option can be applied to.
--
-- * 'ogoMajorEngineVersion' - Indicates the major engine version that the option is available for.
--
-- * 'ogoName' - The name of the option.
--
-- * 'ogoSupportsOptionVersionDowngrade' - If true, you can change the option to an earlier version of the option. This only applies to options that have different versions available.
--
-- * 'ogoDefaultPort' - If the option requires a port, specifies the default port for the option.
--
-- * 'ogoOptionGroupOptionSettings' - The option settings that are available (and the default value) for each option in an option group.
--
-- * 'ogoRequiresAutoMinorEngineVersionUpgrade' - If true, you must enable the Auto Minor Version Upgrade setting for your DB instance before you can use this option. You can enable Auto Minor Version Upgrade when you first create your DB instance, or by modifying your DB instance later.
--
-- * 'ogoPortRequired' - Specifies whether the option requires a port.
--
-- * 'ogoDescription' - The description of the option.
--
-- * 'ogoOptionsDependedOn' - The options that are prerequisites for this option.
--
-- * 'ogoVPCOnly' - If true, you can only use this option with a DB instance that is in a VPC.
optionGroupOption ::
  OptionGroupOption
optionGroupOption =
  OptionGroupOption'
    { _ogoMinimumRequiredMinorEngineVersion =
        Nothing,
      _ogoOptionsConflictsWith = Nothing,
      _ogoPermanent = Nothing,
      _ogoPersistent = Nothing,
      _ogoOptionGroupOptionVersions = Nothing,
      _ogoEngineName = Nothing,
      _ogoMajorEngineVersion = Nothing,
      _ogoName = Nothing,
      _ogoSupportsOptionVersionDowngrade = Nothing,
      _ogoDefaultPort = Nothing,
      _ogoOptionGroupOptionSettings = Nothing,
      _ogoRequiresAutoMinorEngineVersionUpgrade = Nothing,
      _ogoPortRequired = Nothing,
      _ogoDescription = Nothing,
      _ogoOptionsDependedOn = Nothing,
      _ogoVPCOnly = Nothing
    }

-- | The minimum required engine version for the option to be applied.
ogoMinimumRequiredMinorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogoMinimumRequiredMinorEngineVersion = lens _ogoMinimumRequiredMinorEngineVersion (\s a -> s {_ogoMinimumRequiredMinorEngineVersion = a})

-- | The options that conflict with this option.
ogoOptionsConflictsWith :: Lens' OptionGroupOption [Text]
ogoOptionsConflictsWith = lens _ogoOptionsConflictsWith (\s a -> s {_ogoOptionsConflictsWith = a}) . _Default . _Coerce

-- | Permanent options can never be removed from an option group. An option group containing a permanent option can't be removed from a DB instance.
ogoPermanent :: Lens' OptionGroupOption (Maybe Bool)
ogoPermanent = lens _ogoPermanent (\s a -> s {_ogoPermanent = a})

-- | Persistent options can't be removed from an option group while DB instances are associated with the option group. If you disassociate all DB instances from the option group, your can remove the persistent option from the option group.
ogoPersistent :: Lens' OptionGroupOption (Maybe Bool)
ogoPersistent = lens _ogoPersistent (\s a -> s {_ogoPersistent = a})

-- | The versions that are available for the option.
ogoOptionGroupOptionVersions :: Lens' OptionGroupOption [OptionVersion]
ogoOptionGroupOptionVersions = lens _ogoOptionGroupOptionVersions (\s a -> s {_ogoOptionGroupOptionVersions = a}) . _Default . _Coerce

-- | The name of the engine that this option can be applied to.
ogoEngineName :: Lens' OptionGroupOption (Maybe Text)
ogoEngineName = lens _ogoEngineName (\s a -> s {_ogoEngineName = a})

-- | Indicates the major engine version that the option is available for.
ogoMajorEngineVersion :: Lens' OptionGroupOption (Maybe Text)
ogoMajorEngineVersion = lens _ogoMajorEngineVersion (\s a -> s {_ogoMajorEngineVersion = a})

-- | The name of the option.
ogoName :: Lens' OptionGroupOption (Maybe Text)
ogoName = lens _ogoName (\s a -> s {_ogoName = a})

-- | If true, you can change the option to an earlier version of the option. This only applies to options that have different versions available.
ogoSupportsOptionVersionDowngrade :: Lens' OptionGroupOption (Maybe Bool)
ogoSupportsOptionVersionDowngrade = lens _ogoSupportsOptionVersionDowngrade (\s a -> s {_ogoSupportsOptionVersionDowngrade = a})

-- | If the option requires a port, specifies the default port for the option.
ogoDefaultPort :: Lens' OptionGroupOption (Maybe Int)
ogoDefaultPort = lens _ogoDefaultPort (\s a -> s {_ogoDefaultPort = a})

-- | The option settings that are available (and the default value) for each option in an option group.
ogoOptionGroupOptionSettings :: Lens' OptionGroupOption [OptionGroupOptionSetting]
ogoOptionGroupOptionSettings = lens _ogoOptionGroupOptionSettings (\s a -> s {_ogoOptionGroupOptionSettings = a}) . _Default . _Coerce

-- | If true, you must enable the Auto Minor Version Upgrade setting for your DB instance before you can use this option. You can enable Auto Minor Version Upgrade when you first create your DB instance, or by modifying your DB instance later.
ogoRequiresAutoMinorEngineVersionUpgrade :: Lens' OptionGroupOption (Maybe Bool)
ogoRequiresAutoMinorEngineVersionUpgrade = lens _ogoRequiresAutoMinorEngineVersionUpgrade (\s a -> s {_ogoRequiresAutoMinorEngineVersionUpgrade = a})

-- | Specifies whether the option requires a port.
ogoPortRequired :: Lens' OptionGroupOption (Maybe Bool)
ogoPortRequired = lens _ogoPortRequired (\s a -> s {_ogoPortRequired = a})

-- | The description of the option.
ogoDescription :: Lens' OptionGroupOption (Maybe Text)
ogoDescription = lens _ogoDescription (\s a -> s {_ogoDescription = a})

-- | The options that are prerequisites for this option.
ogoOptionsDependedOn :: Lens' OptionGroupOption [Text]
ogoOptionsDependedOn = lens _ogoOptionsDependedOn (\s a -> s {_ogoOptionsDependedOn = a}) . _Default . _Coerce

-- | If true, you can only use this option with a DB instance that is in a VPC.
ogoVPCOnly :: Lens' OptionGroupOption (Maybe Bool)
ogoVPCOnly = lens _ogoVPCOnly (\s a -> s {_ogoVPCOnly = a})

instance FromXML OptionGroupOption where
  parseXML x =
    OptionGroupOption'
      <$> (x .@? "MinimumRequiredMinorEngineVersion")
      <*> ( x .@? "OptionsConflictsWith" .!@ mempty
              >>= may (parseXMLList "OptionConflictName")
          )
      <*> (x .@? "Permanent")
      <*> (x .@? "Persistent")
      <*> ( x .@? "OptionGroupOptionVersions" .!@ mempty
              >>= may (parseXMLList "OptionVersion")
          )
      <*> (x .@? "EngineName")
      <*> (x .@? "MajorEngineVersion")
      <*> (x .@? "Name")
      <*> (x .@? "SupportsOptionVersionDowngrade")
      <*> (x .@? "DefaultPort")
      <*> ( x .@? "OptionGroupOptionSettings" .!@ mempty
              >>= may (parseXMLList "OptionGroupOptionSetting")
          )
      <*> (x .@? "RequiresAutoMinorEngineVersionUpgrade")
      <*> (x .@? "PortRequired")
      <*> (x .@? "Description")
      <*> ( x .@? "OptionsDependedOn" .!@ mempty
              >>= may (parseXMLList "OptionName")
          )
      <*> (x .@? "VpcOnly")

instance Hashable OptionGroupOption

instance NFData OptionGroupOption
