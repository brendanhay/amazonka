{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroupOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.OptionGroupOption
  ( OptionGroupOption (..)
  -- * Smart constructor
  , mkOptionGroupOption
  -- * Lenses
  , ogoDefaultPort
  , ogoDescription
  , ogoEngineName
  , ogoMajorEngineVersion
  , ogoMinimumRequiredMinorEngineVersion
  , ogoName
  , ogoOptionGroupOptionSettings
  , ogoOptionGroupOptionVersions
  , ogoOptionsConflictsWith
  , ogoOptionsDependedOn
  , ogoPermanent
  , ogoPersistent
  , ogoPortRequired
  , ogoRequiresAutoMinorEngineVersionUpgrade
  , ogoSupportsOptionVersionDowngrade
  , ogoVpcOnly
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.OptionGroupOptionSetting as Types
import qualified Network.AWS.RDS.Types.OptionVersion as Types

-- | Available option.
--
-- /See:/ 'mkOptionGroupOption' smart constructor.
data OptionGroupOption = OptionGroupOption'
  { defaultPort :: Core.Maybe Core.Int
    -- ^ If the option requires a port, specifies the default port for the option.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the option.
  , engineName :: Core.Maybe Core.Text
    -- ^ The name of the engine that this option can be applied to.
  , majorEngineVersion :: Core.Maybe Core.Text
    -- ^ Indicates the major engine version that the option is available for.
  , minimumRequiredMinorEngineVersion :: Core.Maybe Core.Text
    -- ^ The minimum required engine version for the option to be applied.
  , name :: Core.Maybe Core.Text
    -- ^ The name of the option.
  , optionGroupOptionSettings :: Core.Maybe [Types.OptionGroupOptionSetting]
    -- ^ The option settings that are available (and the default value) for each option in an option group.
  , optionGroupOptionVersions :: Core.Maybe [Types.OptionVersion]
    -- ^ The versions that are available for the option.
  , optionsConflictsWith :: Core.Maybe [Core.Text]
    -- ^ The options that conflict with this option.
  , optionsDependedOn :: Core.Maybe [Core.Text]
    -- ^ The options that are prerequisites for this option.
  , permanent :: Core.Maybe Core.Bool
    -- ^ Permanent options can never be removed from an option group. An option group containing a permanent option can't be removed from a DB instance.
  , persistent :: Core.Maybe Core.Bool
    -- ^ Persistent options can't be removed from an option group while DB instances are associated with the option group. If you disassociate all DB instances from the option group, your can remove the persistent option from the option group.
  , portRequired :: Core.Maybe Core.Bool
    -- ^ Specifies whether the option requires a port.
  , requiresAutoMinorEngineVersionUpgrade :: Core.Maybe Core.Bool
    -- ^ If true, you must enable the Auto Minor Version Upgrade setting for your DB instance before you can use this option. You can enable Auto Minor Version Upgrade when you first create your DB instance, or by modifying your DB instance later. 
  , supportsOptionVersionDowngrade :: Core.Maybe Core.Bool
    -- ^ If true, you can change the option to an earlier version of the option. This only applies to options that have different versions available. 
  , vpcOnly :: Core.Maybe Core.Bool
    -- ^ If true, you can only use this option with a DB instance that is in a VPC. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionGroupOption' value with any optional fields omitted.
mkOptionGroupOption
    :: OptionGroupOption
mkOptionGroupOption
  = OptionGroupOption'{defaultPort = Core.Nothing,
                       description = Core.Nothing, engineName = Core.Nothing,
                       majorEngineVersion = Core.Nothing,
                       minimumRequiredMinorEngineVersion = Core.Nothing,
                       name = Core.Nothing, optionGroupOptionSettings = Core.Nothing,
                       optionGroupOptionVersions = Core.Nothing,
                       optionsConflictsWith = Core.Nothing,
                       optionsDependedOn = Core.Nothing, permanent = Core.Nothing,
                       persistent = Core.Nothing, portRequired = Core.Nothing,
                       requiresAutoMinorEngineVersionUpgrade = Core.Nothing,
                       supportsOptionVersionDowngrade = Core.Nothing,
                       vpcOnly = Core.Nothing}

-- | If the option requires a port, specifies the default port for the option.
--
-- /Note:/ Consider using 'defaultPort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoDefaultPort :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Int)
ogoDefaultPort = Lens.field @"defaultPort"
{-# INLINEABLE ogoDefaultPort #-}
{-# DEPRECATED defaultPort "Use generic-lens or generic-optics with 'defaultPort' instead"  #-}

-- | The description of the option.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoDescription :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Text)
ogoDescription = Lens.field @"description"
{-# INLINEABLE ogoDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the engine that this option can be applied to.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoEngineName :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Text)
ogoEngineName = Lens.field @"engineName"
{-# INLINEABLE ogoEngineName #-}
{-# DEPRECATED engineName "Use generic-lens or generic-optics with 'engineName' instead"  #-}

-- | Indicates the major engine version that the option is available for.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoMajorEngineVersion :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Text)
ogoMajorEngineVersion = Lens.field @"majorEngineVersion"
{-# INLINEABLE ogoMajorEngineVersion #-}
{-# DEPRECATED majorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead"  #-}

-- | The minimum required engine version for the option to be applied.
--
-- /Note:/ Consider using 'minimumRequiredMinorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoMinimumRequiredMinorEngineVersion :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Text)
ogoMinimumRequiredMinorEngineVersion = Lens.field @"minimumRequiredMinorEngineVersion"
{-# INLINEABLE ogoMinimumRequiredMinorEngineVersion #-}
{-# DEPRECATED minimumRequiredMinorEngineVersion "Use generic-lens or generic-optics with 'minimumRequiredMinorEngineVersion' instead"  #-}

-- | The name of the option.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoName :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Text)
ogoName = Lens.field @"name"
{-# INLINEABLE ogoName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The option settings that are available (and the default value) for each option in an option group.
--
-- /Note:/ Consider using 'optionGroupOptionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoOptionGroupOptionSettings :: Lens.Lens' OptionGroupOption (Core.Maybe [Types.OptionGroupOptionSetting])
ogoOptionGroupOptionSettings = Lens.field @"optionGroupOptionSettings"
{-# INLINEABLE ogoOptionGroupOptionSettings #-}
{-# DEPRECATED optionGroupOptionSettings "Use generic-lens or generic-optics with 'optionGroupOptionSettings' instead"  #-}

-- | The versions that are available for the option.
--
-- /Note:/ Consider using 'optionGroupOptionVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoOptionGroupOptionVersions :: Lens.Lens' OptionGroupOption (Core.Maybe [Types.OptionVersion])
ogoOptionGroupOptionVersions = Lens.field @"optionGroupOptionVersions"
{-# INLINEABLE ogoOptionGroupOptionVersions #-}
{-# DEPRECATED optionGroupOptionVersions "Use generic-lens or generic-optics with 'optionGroupOptionVersions' instead"  #-}

-- | The options that conflict with this option.
--
-- /Note:/ Consider using 'optionsConflictsWith' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoOptionsConflictsWith :: Lens.Lens' OptionGroupOption (Core.Maybe [Core.Text])
ogoOptionsConflictsWith = Lens.field @"optionsConflictsWith"
{-# INLINEABLE ogoOptionsConflictsWith #-}
{-# DEPRECATED optionsConflictsWith "Use generic-lens or generic-optics with 'optionsConflictsWith' instead"  #-}

-- | The options that are prerequisites for this option.
--
-- /Note:/ Consider using 'optionsDependedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoOptionsDependedOn :: Lens.Lens' OptionGroupOption (Core.Maybe [Core.Text])
ogoOptionsDependedOn = Lens.field @"optionsDependedOn"
{-# INLINEABLE ogoOptionsDependedOn #-}
{-# DEPRECATED optionsDependedOn "Use generic-lens or generic-optics with 'optionsDependedOn' instead"  #-}

-- | Permanent options can never be removed from an option group. An option group containing a permanent option can't be removed from a DB instance.
--
-- /Note:/ Consider using 'permanent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoPermanent :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Bool)
ogoPermanent = Lens.field @"permanent"
{-# INLINEABLE ogoPermanent #-}
{-# DEPRECATED permanent "Use generic-lens or generic-optics with 'permanent' instead"  #-}

-- | Persistent options can't be removed from an option group while DB instances are associated with the option group. If you disassociate all DB instances from the option group, your can remove the persistent option from the option group.
--
-- /Note:/ Consider using 'persistent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoPersistent :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Bool)
ogoPersistent = Lens.field @"persistent"
{-# INLINEABLE ogoPersistent #-}
{-# DEPRECATED persistent "Use generic-lens or generic-optics with 'persistent' instead"  #-}

-- | Specifies whether the option requires a port.
--
-- /Note:/ Consider using 'portRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoPortRequired :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Bool)
ogoPortRequired = Lens.field @"portRequired"
{-# INLINEABLE ogoPortRequired #-}
{-# DEPRECATED portRequired "Use generic-lens or generic-optics with 'portRequired' instead"  #-}

-- | If true, you must enable the Auto Minor Version Upgrade setting for your DB instance before you can use this option. You can enable Auto Minor Version Upgrade when you first create your DB instance, or by modifying your DB instance later. 
--
-- /Note:/ Consider using 'requiresAutoMinorEngineVersionUpgrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoRequiresAutoMinorEngineVersionUpgrade :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Bool)
ogoRequiresAutoMinorEngineVersionUpgrade = Lens.field @"requiresAutoMinorEngineVersionUpgrade"
{-# INLINEABLE ogoRequiresAutoMinorEngineVersionUpgrade #-}
{-# DEPRECATED requiresAutoMinorEngineVersionUpgrade "Use generic-lens or generic-optics with 'requiresAutoMinorEngineVersionUpgrade' instead"  #-}

-- | If true, you can change the option to an earlier version of the option. This only applies to options that have different versions available. 
--
-- /Note:/ Consider using 'supportsOptionVersionDowngrade' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoSupportsOptionVersionDowngrade :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Bool)
ogoSupportsOptionVersionDowngrade = Lens.field @"supportsOptionVersionDowngrade"
{-# INLINEABLE ogoSupportsOptionVersionDowngrade #-}
{-# DEPRECATED supportsOptionVersionDowngrade "Use generic-lens or generic-optics with 'supportsOptionVersionDowngrade' instead"  #-}

-- | If true, you can only use this option with a DB instance that is in a VPC. 
--
-- /Note:/ Consider using 'vpcOnly' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogoVpcOnly :: Lens.Lens' OptionGroupOption (Core.Maybe Core.Bool)
ogoVpcOnly = Lens.field @"vpcOnly"
{-# INLINEABLE ogoVpcOnly #-}
{-# DEPRECATED vpcOnly "Use generic-lens or generic-optics with 'vpcOnly' instead"  #-}

instance Core.FromXML OptionGroupOption where
        parseXML x
          = OptionGroupOption' Core.<$>
              (x Core..@? "DefaultPort") Core.<*> x Core..@? "Description"
                Core.<*> x Core..@? "EngineName"
                Core.<*> x Core..@? "MajorEngineVersion"
                Core.<*> x Core..@? "MinimumRequiredMinorEngineVersion"
                Core.<*> x Core..@? "Name"
                Core.<*>
                x Core..@? "OptionGroupOptionSettings" Core..<@>
                  Core.parseXMLList "OptionGroupOptionSetting"
                Core.<*>
                x Core..@? "OptionGroupOptionVersions" Core..<@>
                  Core.parseXMLList "OptionVersion"
                Core.<*>
                x Core..@? "OptionsConflictsWith" Core..<@>
                  Core.parseXMLList "OptionConflictName"
                Core.<*>
                x Core..@? "OptionsDependedOn" Core..<@>
                  Core.parseXMLList "OptionName"
                Core.<*> x Core..@? "Permanent"
                Core.<*> x Core..@? "Persistent"
                Core.<*> x Core..@? "PortRequired"
                Core.<*> x Core..@? "RequiresAutoMinorEngineVersionUpgrade"
                Core.<*> x Core..@? "SupportsOptionVersionDowngrade"
                Core.<*> x Core..@? "VpcOnly"
