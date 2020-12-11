-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroupOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionGroupOption
  ( OptionGroupOption (..),

    -- * Smart constructor
    mkOptionGroupOption,

    -- * Lenses
    ogoMinimumRequiredMinorEngineVersion,
    ogoOptionsConflictsWith,
    ogoPermanent,
    ogoPersistent,
    ogoOptionGroupOptionVersions,
    ogoEngineName,
    ogoMajorEngineVersion,
    ogoName,
    ogoSupportsOptionVersionDowngrade,
    ogoDefaultPort,
    ogoOptionGroupOptionSettings,
    ogoRequiresAutoMinorEngineVersionUpgrade,
    ogoPortRequired,
    ogoDescription,
    ogoOptionsDependedOn,
    ogoVPCOnly,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.OptionGroupOptionSetting
import Network.AWS.RDS.Types.OptionVersion

-- | Available option.
--
-- /See:/ 'mkOptionGroupOption' smart constructor.
data OptionGroupOption = OptionGroupOption'
  { minimumRequiredMinorEngineVersion ::
      Lude.Maybe Lude.Text,
    optionsConflictsWith :: Lude.Maybe [Lude.Text],
    permanent :: Lude.Maybe Lude.Bool,
    persistent :: Lude.Maybe Lude.Bool,
    optionGroupOptionVersions :: Lude.Maybe [OptionVersion],
    engineName :: Lude.Maybe Lude.Text,
    majorEngineVersion :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    supportsOptionVersionDowngrade :: Lude.Maybe Lude.Bool,
    defaultPort :: Lude.Maybe Lude.Int,
    optionGroupOptionSettings ::
      Lude.Maybe [OptionGroupOptionSetting],
    requiresAutoMinorEngineVersionUpgrade ::
      Lude.Maybe Lude.Bool,
    portRequired :: Lude.Maybe Lude.Bool,
    description :: Lude.Maybe Lude.Text,
    optionsDependedOn :: Lude.Maybe [Lude.Text],
    vpcOnly :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionGroupOption' with the minimum fields required to make a request.
--
-- * 'defaultPort' - If the option requires a port, specifies the default port for the option.
-- * 'description' - The description of the option.
-- * 'engineName' - The name of the engine that this option can be applied to.
-- * 'majorEngineVersion' - Indicates the major engine version that the option is available for.
-- * 'minimumRequiredMinorEngineVersion' - The minimum required engine version for the option to be applied.
-- * 'name' - The name of the option.
-- * 'optionGroupOptionSettings' - The option settings that are available (and the default value) for each option in an option group.
-- * 'optionGroupOptionVersions' - The versions that are available for the option.
-- * 'optionsConflictsWith' - The options that conflict with this option.
-- * 'optionsDependedOn' - The options that are prerequisites for this option.
-- * 'permanent' - Permanent options can never be removed from an option group. An option group containing a permanent option can't be removed from a DB instance.
-- * 'persistent' - Persistent options can't be removed from an option group while DB instances are associated with the option group. If you disassociate all DB instances from the option group, your can remove the persistent option from the option group.
-- * 'portRequired' - Specifies whether the option requires a port.
-- * 'requiresAutoMinorEngineVersionUpgrade' - If true, you must enable the Auto Minor Version Upgrade setting for your DB instance before you can use this option. You can enable Auto Minor Version Upgrade when you first create your DB instance, or by modifying your DB instance later.
-- * 'supportsOptionVersionDowngrade' - If true, you can change the option to an earlier version of the option. This only applies to options that have different versions available.
-- * 'vpcOnly' - If true, you can only use this option with a DB instance that is in a VPC.
mkOptionGroupOption ::
  OptionGroupOption
mkOptionGroupOption =
  OptionGroupOption'
    { minimumRequiredMinorEngineVersion =
        Lude.Nothing,
      optionsConflictsWith = Lude.Nothing,
      permanent = Lude.Nothing,
      persistent = Lude.Nothing,
      optionGroupOptionVersions = Lude.Nothing,
      engineName = Lude.Nothing,
      majorEngineVersion = Lude.Nothing,
      name = Lude.Nothing,
      supportsOptionVersionDowngrade = Lude.Nothing,
      defaultPort = Lude.Nothing,
      optionGroupOptionSettings = Lude.Nothing,
      requiresAutoMinorEngineVersionUpgrade = Lude.Nothing,
      portRequired = Lude.Nothing,
      description = Lude.Nothing,
      optionsDependedOn = Lude.Nothing,
      vpcOnly = Lude.Nothing
    }

-- | The minimum required engine version for the option to be applied.
--
-- /Note:/ Consider using 'minimumRequiredMinorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoMinimumRequiredMinorEngineVersion :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Text)
ogoMinimumRequiredMinorEngineVersion = Lens.lens (minimumRequiredMinorEngineVersion :: OptionGroupOption -> Lude.Maybe Lude.Text) (\s a -> s {minimumRequiredMinorEngineVersion = a} :: OptionGroupOption)
{-# DEPRECATED ogoMinimumRequiredMinorEngineVersion "Use generic-lens or generic-optics with 'minimumRequiredMinorEngineVersion' instead." #-}

-- | The options that conflict with this option.
--
-- /Note:/ Consider using 'optionsConflictsWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoOptionsConflictsWith :: Lens.Lens' OptionGroupOption (Lude.Maybe [Lude.Text])
ogoOptionsConflictsWith = Lens.lens (optionsConflictsWith :: OptionGroupOption -> Lude.Maybe [Lude.Text]) (\s a -> s {optionsConflictsWith = a} :: OptionGroupOption)
{-# DEPRECATED ogoOptionsConflictsWith "Use generic-lens or generic-optics with 'optionsConflictsWith' instead." #-}

-- | Permanent options can never be removed from an option group. An option group containing a permanent option can't be removed from a DB instance.
--
-- /Note:/ Consider using 'permanent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoPermanent :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Bool)
ogoPermanent = Lens.lens (permanent :: OptionGroupOption -> Lude.Maybe Lude.Bool) (\s a -> s {permanent = a} :: OptionGroupOption)
{-# DEPRECATED ogoPermanent "Use generic-lens or generic-optics with 'permanent' instead." #-}

-- | Persistent options can't be removed from an option group while DB instances are associated with the option group. If you disassociate all DB instances from the option group, your can remove the persistent option from the option group.
--
-- /Note:/ Consider using 'persistent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoPersistent :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Bool)
ogoPersistent = Lens.lens (persistent :: OptionGroupOption -> Lude.Maybe Lude.Bool) (\s a -> s {persistent = a} :: OptionGroupOption)
{-# DEPRECATED ogoPersistent "Use generic-lens or generic-optics with 'persistent' instead." #-}

-- | The versions that are available for the option.
--
-- /Note:/ Consider using 'optionGroupOptionVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoOptionGroupOptionVersions :: Lens.Lens' OptionGroupOption (Lude.Maybe [OptionVersion])
ogoOptionGroupOptionVersions = Lens.lens (optionGroupOptionVersions :: OptionGroupOption -> Lude.Maybe [OptionVersion]) (\s a -> s {optionGroupOptionVersions = a} :: OptionGroupOption)
{-# DEPRECATED ogoOptionGroupOptionVersions "Use generic-lens or generic-optics with 'optionGroupOptionVersions' instead." #-}

-- | The name of the engine that this option can be applied to.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoEngineName :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Text)
ogoEngineName = Lens.lens (engineName :: OptionGroupOption -> Lude.Maybe Lude.Text) (\s a -> s {engineName = a} :: OptionGroupOption)
{-# DEPRECATED ogoEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | Indicates the major engine version that the option is available for.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoMajorEngineVersion :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Text)
ogoMajorEngineVersion = Lens.lens (majorEngineVersion :: OptionGroupOption -> Lude.Maybe Lude.Text) (\s a -> s {majorEngineVersion = a} :: OptionGroupOption)
{-# DEPRECATED ogoMajorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead." #-}

-- | The name of the option.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoName :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Text)
ogoName = Lens.lens (name :: OptionGroupOption -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: OptionGroupOption)
{-# DEPRECATED ogoName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | If true, you can change the option to an earlier version of the option. This only applies to options that have different versions available.
--
-- /Note:/ Consider using 'supportsOptionVersionDowngrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoSupportsOptionVersionDowngrade :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Bool)
ogoSupportsOptionVersionDowngrade = Lens.lens (supportsOptionVersionDowngrade :: OptionGroupOption -> Lude.Maybe Lude.Bool) (\s a -> s {supportsOptionVersionDowngrade = a} :: OptionGroupOption)
{-# DEPRECATED ogoSupportsOptionVersionDowngrade "Use generic-lens or generic-optics with 'supportsOptionVersionDowngrade' instead." #-}

-- | If the option requires a port, specifies the default port for the option.
--
-- /Note:/ Consider using 'defaultPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoDefaultPort :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Int)
ogoDefaultPort = Lens.lens (defaultPort :: OptionGroupOption -> Lude.Maybe Lude.Int) (\s a -> s {defaultPort = a} :: OptionGroupOption)
{-# DEPRECATED ogoDefaultPort "Use generic-lens or generic-optics with 'defaultPort' instead." #-}

-- | The option settings that are available (and the default value) for each option in an option group.
--
-- /Note:/ Consider using 'optionGroupOptionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoOptionGroupOptionSettings :: Lens.Lens' OptionGroupOption (Lude.Maybe [OptionGroupOptionSetting])
ogoOptionGroupOptionSettings = Lens.lens (optionGroupOptionSettings :: OptionGroupOption -> Lude.Maybe [OptionGroupOptionSetting]) (\s a -> s {optionGroupOptionSettings = a} :: OptionGroupOption)
{-# DEPRECATED ogoOptionGroupOptionSettings "Use generic-lens or generic-optics with 'optionGroupOptionSettings' instead." #-}

-- | If true, you must enable the Auto Minor Version Upgrade setting for your DB instance before you can use this option. You can enable Auto Minor Version Upgrade when you first create your DB instance, or by modifying your DB instance later.
--
-- /Note:/ Consider using 'requiresAutoMinorEngineVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoRequiresAutoMinorEngineVersionUpgrade :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Bool)
ogoRequiresAutoMinorEngineVersionUpgrade = Lens.lens (requiresAutoMinorEngineVersionUpgrade :: OptionGroupOption -> Lude.Maybe Lude.Bool) (\s a -> s {requiresAutoMinorEngineVersionUpgrade = a} :: OptionGroupOption)
{-# DEPRECATED ogoRequiresAutoMinorEngineVersionUpgrade "Use generic-lens or generic-optics with 'requiresAutoMinorEngineVersionUpgrade' instead." #-}

-- | Specifies whether the option requires a port.
--
-- /Note:/ Consider using 'portRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoPortRequired :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Bool)
ogoPortRequired = Lens.lens (portRequired :: OptionGroupOption -> Lude.Maybe Lude.Bool) (\s a -> s {portRequired = a} :: OptionGroupOption)
{-# DEPRECATED ogoPortRequired "Use generic-lens or generic-optics with 'portRequired' instead." #-}

-- | The description of the option.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoDescription :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Text)
ogoDescription = Lens.lens (description :: OptionGroupOption -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: OptionGroupOption)
{-# DEPRECATED ogoDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The options that are prerequisites for this option.
--
-- /Note:/ Consider using 'optionsDependedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoOptionsDependedOn :: Lens.Lens' OptionGroupOption (Lude.Maybe [Lude.Text])
ogoOptionsDependedOn = Lens.lens (optionsDependedOn :: OptionGroupOption -> Lude.Maybe [Lude.Text]) (\s a -> s {optionsDependedOn = a} :: OptionGroupOption)
{-# DEPRECATED ogoOptionsDependedOn "Use generic-lens or generic-optics with 'optionsDependedOn' instead." #-}

-- | If true, you can only use this option with a DB instance that is in a VPC.
--
-- /Note:/ Consider using 'vpcOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoVPCOnly :: Lens.Lens' OptionGroupOption (Lude.Maybe Lude.Bool)
ogoVPCOnly = Lens.lens (vpcOnly :: OptionGroupOption -> Lude.Maybe Lude.Bool) (\s a -> s {vpcOnly = a} :: OptionGroupOption)
{-# DEPRECATED ogoVPCOnly "Use generic-lens or generic-optics with 'vpcOnly' instead." #-}

instance Lude.FromXML OptionGroupOption where
  parseXML x =
    OptionGroupOption'
      Lude.<$> (x Lude..@? "MinimumRequiredMinorEngineVersion")
      Lude.<*> ( x Lude..@? "OptionsConflictsWith" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "OptionConflictName")
               )
      Lude.<*> (x Lude..@? "Permanent")
      Lude.<*> (x Lude..@? "Persistent")
      Lude.<*> ( x Lude..@? "OptionGroupOptionVersions" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "OptionVersion")
               )
      Lude.<*> (x Lude..@? "EngineName")
      Lude.<*> (x Lude..@? "MajorEngineVersion")
      Lude.<*> (x Lude..@? "Name")
      Lude.<*> (x Lude..@? "SupportsOptionVersionDowngrade")
      Lude.<*> (x Lude..@? "DefaultPort")
      Lude.<*> ( x Lude..@? "OptionGroupOptionSettings" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "OptionGroupOptionSetting")
               )
      Lude.<*> (x Lude..@? "RequiresAutoMinorEngineVersionUpgrade")
      Lude.<*> (x Lude..@? "PortRequired")
      Lude.<*> (x Lude..@? "Description")
      Lude.<*> ( x Lude..@? "OptionsDependedOn" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "OptionName")
               )
      Lude.<*> (x Lude..@? "VpcOnly")
