{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionConfiguration
  ( OptionConfiguration (..),

    -- * Smart constructor
    mkOptionConfiguration,

    -- * Lenses
    ocOptionName,
    ocDBSecurityGroupMemberships,
    ocOptionSettings,
    ocOptionVersion,
    ocPort,
    ocVpcSecurityGroupMemberships,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.OptionSetting as Types
import qualified Network.AWS.RDS.Types.String as Types

-- | A list of all available options
--
-- /See:/ 'mkOptionConfiguration' smart constructor.
data OptionConfiguration = OptionConfiguration'
  { -- | The configuration of options to include in a group.
    optionName :: Types.String,
    -- | A list of DBSecurityGroupMembership name strings used for this option.
    dBSecurityGroupMemberships :: Core.Maybe [Types.String],
    -- | The option settings to include in an option group.
    optionSettings :: Core.Maybe [Types.OptionSetting],
    -- | The version for the option.
    optionVersion :: Core.Maybe Types.String,
    -- | The optional port for the option.
    port :: Core.Maybe Core.Int,
    -- | A list of VpcSecurityGroupMembership name strings used for this option.
    vpcSecurityGroupMemberships :: Core.Maybe [Types.String]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionConfiguration' value with any optional fields omitted.
mkOptionConfiguration ::
  -- | 'optionName'
  Types.String ->
  OptionConfiguration
mkOptionConfiguration optionName =
  OptionConfiguration'
    { optionName,
      dBSecurityGroupMemberships = Core.Nothing,
      optionSettings = Core.Nothing,
      optionVersion = Core.Nothing,
      port = Core.Nothing,
      vpcSecurityGroupMemberships = Core.Nothing
    }

-- | The configuration of options to include in a group.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocOptionName :: Lens.Lens' OptionConfiguration Types.String
ocOptionName = Lens.field @"optionName"
{-# DEPRECATED ocOptionName "Use generic-lens or generic-optics with 'optionName' instead." #-}

-- | A list of DBSecurityGroupMembership name strings used for this option.
--
-- /Note:/ Consider using 'dBSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocDBSecurityGroupMemberships :: Lens.Lens' OptionConfiguration (Core.Maybe [Types.String])
ocDBSecurityGroupMemberships = Lens.field @"dBSecurityGroupMemberships"
{-# DEPRECATED ocDBSecurityGroupMemberships "Use generic-lens or generic-optics with 'dBSecurityGroupMemberships' instead." #-}

-- | The option settings to include in an option group.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocOptionSettings :: Lens.Lens' OptionConfiguration (Core.Maybe [Types.OptionSetting])
ocOptionSettings = Lens.field @"optionSettings"
{-# DEPRECATED ocOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | The version for the option.
--
-- /Note:/ Consider using 'optionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocOptionVersion :: Lens.Lens' OptionConfiguration (Core.Maybe Types.String)
ocOptionVersion = Lens.field @"optionVersion"
{-# DEPRECATED ocOptionVersion "Use generic-lens or generic-optics with 'optionVersion' instead." #-}

-- | The optional port for the option.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocPort :: Lens.Lens' OptionConfiguration (Core.Maybe Core.Int)
ocPort = Lens.field @"port"
{-# DEPRECATED ocPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | A list of VpcSecurityGroupMembership name strings used for this option.
--
-- /Note:/ Consider using 'vpcSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocVpcSecurityGroupMemberships :: Lens.Lens' OptionConfiguration (Core.Maybe [Types.String])
ocVpcSecurityGroupMemberships = Lens.field @"vpcSecurityGroupMemberships"
{-# DEPRECATED ocVpcSecurityGroupMemberships "Use generic-lens or generic-optics with 'vpcSecurityGroupMemberships' instead." #-}
