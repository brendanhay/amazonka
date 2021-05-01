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
-- Module      : Network.AWS.RDS.Types.OptionGroupOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionGroupOption where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.OptionGroupOptionSetting
import Network.AWS.RDS.Types.OptionVersion

-- | Available option.
--
-- /See:/ 'newOptionGroupOption' smart constructor.
data OptionGroupOption = OptionGroupOption'
  { -- | The options that conflict with this option.
    optionsConflictsWith :: Prelude.Maybe [Prelude.Text],
    -- | If true, you can only use this option with a DB instance that is in a
    -- VPC.
    vpcOnly :: Prelude.Maybe Prelude.Bool,
    -- | The name of the engine that this option can be applied to.
    engineName :: Prelude.Maybe Prelude.Text,
    -- | The versions that are available for the option.
    optionGroupOptionVersions :: Prelude.Maybe [OptionVersion],
    -- | If the option requires a port, specifies the default port for the
    -- option.
    defaultPort :: Prelude.Maybe Prelude.Int,
    -- | If true, you must enable the Auto Minor Version Upgrade setting for your
    -- DB instance before you can use this option. You can enable Auto Minor
    -- Version Upgrade when you first create your DB instance, or by modifying
    -- your DB instance later.
    requiresAutoMinorEngineVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The option settings that are available (and the default value) for each
    -- option in an option group.
    optionGroupOptionSettings :: Prelude.Maybe [OptionGroupOptionSetting],
    -- | Indicates the major engine version that the option is available for.
    majorEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the option.
    name :: Prelude.Maybe Prelude.Text,
    -- | The minimum required engine version for the option to be applied.
    minimumRequiredMinorEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | The options that are prerequisites for this option.
    optionsDependedOn :: Prelude.Maybe [Prelude.Text],
    -- | The description of the option.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the option requires a port.
    portRequired :: Prelude.Maybe Prelude.Bool,
    -- | Persistent options can\'t be removed from an option group while DB
    -- instances are associated with the option group. If you disassociate all
    -- DB instances from the option group, your can remove the persistent
    -- option from the option group.
    persistent :: Prelude.Maybe Prelude.Bool,
    -- | Permanent options can never be removed from an option group. An option
    -- group containing a permanent option can\'t be removed from a DB
    -- instance.
    permanent :: Prelude.Maybe Prelude.Bool,
    -- | If true, you can change the option to an earlier version of the option.
    -- This only applies to options that have different versions available.
    supportsOptionVersionDowngrade :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'OptionGroupOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionsConflictsWith', 'optionGroupOption_optionsConflictsWith' - The options that conflict with this option.
--
-- 'vpcOnly', 'optionGroupOption_vpcOnly' - If true, you can only use this option with a DB instance that is in a
-- VPC.
--
-- 'engineName', 'optionGroupOption_engineName' - The name of the engine that this option can be applied to.
--
-- 'optionGroupOptionVersions', 'optionGroupOption_optionGroupOptionVersions' - The versions that are available for the option.
--
-- 'defaultPort', 'optionGroupOption_defaultPort' - If the option requires a port, specifies the default port for the
-- option.
--
-- 'requiresAutoMinorEngineVersionUpgrade', 'optionGroupOption_requiresAutoMinorEngineVersionUpgrade' - If true, you must enable the Auto Minor Version Upgrade setting for your
-- DB instance before you can use this option. You can enable Auto Minor
-- Version Upgrade when you first create your DB instance, or by modifying
-- your DB instance later.
--
-- 'optionGroupOptionSettings', 'optionGroupOption_optionGroupOptionSettings' - The option settings that are available (and the default value) for each
-- option in an option group.
--
-- 'majorEngineVersion', 'optionGroupOption_majorEngineVersion' - Indicates the major engine version that the option is available for.
--
-- 'name', 'optionGroupOption_name' - The name of the option.
--
-- 'minimumRequiredMinorEngineVersion', 'optionGroupOption_minimumRequiredMinorEngineVersion' - The minimum required engine version for the option to be applied.
--
-- 'optionsDependedOn', 'optionGroupOption_optionsDependedOn' - The options that are prerequisites for this option.
--
-- 'description', 'optionGroupOption_description' - The description of the option.
--
-- 'portRequired', 'optionGroupOption_portRequired' - Specifies whether the option requires a port.
--
-- 'persistent', 'optionGroupOption_persistent' - Persistent options can\'t be removed from an option group while DB
-- instances are associated with the option group. If you disassociate all
-- DB instances from the option group, your can remove the persistent
-- option from the option group.
--
-- 'permanent', 'optionGroupOption_permanent' - Permanent options can never be removed from an option group. An option
-- group containing a permanent option can\'t be removed from a DB
-- instance.
--
-- 'supportsOptionVersionDowngrade', 'optionGroupOption_supportsOptionVersionDowngrade' - If true, you can change the option to an earlier version of the option.
-- This only applies to options that have different versions available.
newOptionGroupOption ::
  OptionGroupOption
newOptionGroupOption =
  OptionGroupOption'
    { optionsConflictsWith =
        Prelude.Nothing,
      vpcOnly = Prelude.Nothing,
      engineName = Prelude.Nothing,
      optionGroupOptionVersions = Prelude.Nothing,
      defaultPort = Prelude.Nothing,
      requiresAutoMinorEngineVersionUpgrade =
        Prelude.Nothing,
      optionGroupOptionSettings = Prelude.Nothing,
      majorEngineVersion = Prelude.Nothing,
      name = Prelude.Nothing,
      minimumRequiredMinorEngineVersion = Prelude.Nothing,
      optionsDependedOn = Prelude.Nothing,
      description = Prelude.Nothing,
      portRequired = Prelude.Nothing,
      persistent = Prelude.Nothing,
      permanent = Prelude.Nothing,
      supportsOptionVersionDowngrade = Prelude.Nothing
    }

-- | The options that conflict with this option.
optionGroupOption_optionsConflictsWith :: Lens.Lens' OptionGroupOption (Prelude.Maybe [Prelude.Text])
optionGroupOption_optionsConflictsWith = Lens.lens (\OptionGroupOption' {optionsConflictsWith} -> optionsConflictsWith) (\s@OptionGroupOption' {} a -> s {optionsConflictsWith = a} :: OptionGroupOption) Prelude.. Lens.mapping Prelude._Coerce

-- | If true, you can only use this option with a DB instance that is in a
-- VPC.
optionGroupOption_vpcOnly :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Bool)
optionGroupOption_vpcOnly = Lens.lens (\OptionGroupOption' {vpcOnly} -> vpcOnly) (\s@OptionGroupOption' {} a -> s {vpcOnly = a} :: OptionGroupOption)

-- | The name of the engine that this option can be applied to.
optionGroupOption_engineName :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Text)
optionGroupOption_engineName = Lens.lens (\OptionGroupOption' {engineName} -> engineName) (\s@OptionGroupOption' {} a -> s {engineName = a} :: OptionGroupOption)

-- | The versions that are available for the option.
optionGroupOption_optionGroupOptionVersions :: Lens.Lens' OptionGroupOption (Prelude.Maybe [OptionVersion])
optionGroupOption_optionGroupOptionVersions = Lens.lens (\OptionGroupOption' {optionGroupOptionVersions} -> optionGroupOptionVersions) (\s@OptionGroupOption' {} a -> s {optionGroupOptionVersions = a} :: OptionGroupOption) Prelude.. Lens.mapping Prelude._Coerce

-- | If the option requires a port, specifies the default port for the
-- option.
optionGroupOption_defaultPort :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Int)
optionGroupOption_defaultPort = Lens.lens (\OptionGroupOption' {defaultPort} -> defaultPort) (\s@OptionGroupOption' {} a -> s {defaultPort = a} :: OptionGroupOption)

-- | If true, you must enable the Auto Minor Version Upgrade setting for your
-- DB instance before you can use this option. You can enable Auto Minor
-- Version Upgrade when you first create your DB instance, or by modifying
-- your DB instance later.
optionGroupOption_requiresAutoMinorEngineVersionUpgrade :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Bool)
optionGroupOption_requiresAutoMinorEngineVersionUpgrade = Lens.lens (\OptionGroupOption' {requiresAutoMinorEngineVersionUpgrade} -> requiresAutoMinorEngineVersionUpgrade) (\s@OptionGroupOption' {} a -> s {requiresAutoMinorEngineVersionUpgrade = a} :: OptionGroupOption)

-- | The option settings that are available (and the default value) for each
-- option in an option group.
optionGroupOption_optionGroupOptionSettings :: Lens.Lens' OptionGroupOption (Prelude.Maybe [OptionGroupOptionSetting])
optionGroupOption_optionGroupOptionSettings = Lens.lens (\OptionGroupOption' {optionGroupOptionSettings} -> optionGroupOptionSettings) (\s@OptionGroupOption' {} a -> s {optionGroupOptionSettings = a} :: OptionGroupOption) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates the major engine version that the option is available for.
optionGroupOption_majorEngineVersion :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Text)
optionGroupOption_majorEngineVersion = Lens.lens (\OptionGroupOption' {majorEngineVersion} -> majorEngineVersion) (\s@OptionGroupOption' {} a -> s {majorEngineVersion = a} :: OptionGroupOption)

-- | The name of the option.
optionGroupOption_name :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Text)
optionGroupOption_name = Lens.lens (\OptionGroupOption' {name} -> name) (\s@OptionGroupOption' {} a -> s {name = a} :: OptionGroupOption)

-- | The minimum required engine version for the option to be applied.
optionGroupOption_minimumRequiredMinorEngineVersion :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Text)
optionGroupOption_minimumRequiredMinorEngineVersion = Lens.lens (\OptionGroupOption' {minimumRequiredMinorEngineVersion} -> minimumRequiredMinorEngineVersion) (\s@OptionGroupOption' {} a -> s {minimumRequiredMinorEngineVersion = a} :: OptionGroupOption)

-- | The options that are prerequisites for this option.
optionGroupOption_optionsDependedOn :: Lens.Lens' OptionGroupOption (Prelude.Maybe [Prelude.Text])
optionGroupOption_optionsDependedOn = Lens.lens (\OptionGroupOption' {optionsDependedOn} -> optionsDependedOn) (\s@OptionGroupOption' {} a -> s {optionsDependedOn = a} :: OptionGroupOption) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the option.
optionGroupOption_description :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Text)
optionGroupOption_description = Lens.lens (\OptionGroupOption' {description} -> description) (\s@OptionGroupOption' {} a -> s {description = a} :: OptionGroupOption)

-- | Specifies whether the option requires a port.
optionGroupOption_portRequired :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Bool)
optionGroupOption_portRequired = Lens.lens (\OptionGroupOption' {portRequired} -> portRequired) (\s@OptionGroupOption' {} a -> s {portRequired = a} :: OptionGroupOption)

-- | Persistent options can\'t be removed from an option group while DB
-- instances are associated with the option group. If you disassociate all
-- DB instances from the option group, your can remove the persistent
-- option from the option group.
optionGroupOption_persistent :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Bool)
optionGroupOption_persistent = Lens.lens (\OptionGroupOption' {persistent} -> persistent) (\s@OptionGroupOption' {} a -> s {persistent = a} :: OptionGroupOption)

-- | Permanent options can never be removed from an option group. An option
-- group containing a permanent option can\'t be removed from a DB
-- instance.
optionGroupOption_permanent :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Bool)
optionGroupOption_permanent = Lens.lens (\OptionGroupOption' {permanent} -> permanent) (\s@OptionGroupOption' {} a -> s {permanent = a} :: OptionGroupOption)

-- | If true, you can change the option to an earlier version of the option.
-- This only applies to options that have different versions available.
optionGroupOption_supportsOptionVersionDowngrade :: Lens.Lens' OptionGroupOption (Prelude.Maybe Prelude.Bool)
optionGroupOption_supportsOptionVersionDowngrade = Lens.lens (\OptionGroupOption' {supportsOptionVersionDowngrade} -> supportsOptionVersionDowngrade) (\s@OptionGroupOption' {} a -> s {supportsOptionVersionDowngrade = a} :: OptionGroupOption)

instance Prelude.FromXML OptionGroupOption where
  parseXML x =
    OptionGroupOption'
      Prelude.<$> ( x Prelude..@? "OptionsConflictsWith"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "OptionConflictName")
                  )
      Prelude.<*> (x Prelude..@? "VpcOnly")
      Prelude.<*> (x Prelude..@? "EngineName")
      Prelude.<*> ( x Prelude..@? "OptionGroupOptionVersions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "OptionVersion")
                  )
      Prelude.<*> (x Prelude..@? "DefaultPort")
      Prelude.<*> ( x
                      Prelude..@? "RequiresAutoMinorEngineVersionUpgrade"
                  )
      Prelude.<*> ( x Prelude..@? "OptionGroupOptionSettings"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "OptionGroupOptionSetting")
                  )
      Prelude.<*> (x Prelude..@? "MajorEngineVersion")
      Prelude.<*> (x Prelude..@? "Name")
      Prelude.<*> (x Prelude..@? "MinimumRequiredMinorEngineVersion")
      Prelude.<*> ( x Prelude..@? "OptionsDependedOn"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "OptionName")
                  )
      Prelude.<*> (x Prelude..@? "Description")
      Prelude.<*> (x Prelude..@? "PortRequired")
      Prelude.<*> (x Prelude..@? "Persistent")
      Prelude.<*> (x Prelude..@? "Permanent")
      Prelude.<*> (x Prelude..@? "SupportsOptionVersionDowngrade")

instance Prelude.Hashable OptionGroupOption

instance Prelude.NFData OptionGroupOption
