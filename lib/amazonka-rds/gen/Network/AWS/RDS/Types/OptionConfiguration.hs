{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.OptionConfiguration
  ( OptionConfiguration (..)
  -- * Smart constructor
  , mkOptionConfiguration
  -- * Lenses
  , ocOptionName
  , ocDBSecurityGroupMemberships
  , ocOptionSettings
  , ocOptionVersion
  , ocPort
  , ocVpcSecurityGroupMemberships
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.OptionSetting as Types

-- | A list of all available options
--
-- /See:/ 'mkOptionConfiguration' smart constructor.
data OptionConfiguration = OptionConfiguration'
  { optionName :: Core.Text
    -- ^ The configuration of options to include in a group.
  , dBSecurityGroupMemberships :: Core.Maybe [Core.Text]
    -- ^ A list of DBSecurityGroupMembership name strings used for this option.
  , optionSettings :: Core.Maybe [Types.OptionSetting]
    -- ^ The option settings to include in an option group.
  , optionVersion :: Core.Maybe Core.Text
    -- ^ The version for the option.
  , port :: Core.Maybe Core.Int
    -- ^ The optional port for the option.
  , vpcSecurityGroupMemberships :: Core.Maybe [Core.Text]
    -- ^ A list of VpcSecurityGroupMembership name strings used for this option.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionConfiguration' value with any optional fields omitted.
mkOptionConfiguration
    :: Core.Text -- ^ 'optionName'
    -> OptionConfiguration
mkOptionConfiguration optionName
  = OptionConfiguration'{optionName,
                         dBSecurityGroupMemberships = Core.Nothing,
                         optionSettings = Core.Nothing, optionVersion = Core.Nothing,
                         port = Core.Nothing, vpcSecurityGroupMemberships = Core.Nothing}

-- | The configuration of options to include in a group.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocOptionName :: Lens.Lens' OptionConfiguration Core.Text
ocOptionName = Lens.field @"optionName"
{-# INLINEABLE ocOptionName #-}
{-# DEPRECATED optionName "Use generic-lens or generic-optics with 'optionName' instead"  #-}

-- | A list of DBSecurityGroupMembership name strings used for this option.
--
-- /Note:/ Consider using 'dBSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocDBSecurityGroupMemberships :: Lens.Lens' OptionConfiguration (Core.Maybe [Core.Text])
ocDBSecurityGroupMemberships = Lens.field @"dBSecurityGroupMemberships"
{-# INLINEABLE ocDBSecurityGroupMemberships #-}
{-# DEPRECATED dBSecurityGroupMemberships "Use generic-lens or generic-optics with 'dBSecurityGroupMemberships' instead"  #-}

-- | The option settings to include in an option group.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocOptionSettings :: Lens.Lens' OptionConfiguration (Core.Maybe [Types.OptionSetting])
ocOptionSettings = Lens.field @"optionSettings"
{-# INLINEABLE ocOptionSettings #-}
{-# DEPRECATED optionSettings "Use generic-lens or generic-optics with 'optionSettings' instead"  #-}

-- | The version for the option.
--
-- /Note:/ Consider using 'optionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocOptionVersion :: Lens.Lens' OptionConfiguration (Core.Maybe Core.Text)
ocOptionVersion = Lens.field @"optionVersion"
{-# INLINEABLE ocOptionVersion #-}
{-# DEPRECATED optionVersion "Use generic-lens or generic-optics with 'optionVersion' instead"  #-}

-- | The optional port for the option.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocPort :: Lens.Lens' OptionConfiguration (Core.Maybe Core.Int)
ocPort = Lens.field @"port"
{-# INLINEABLE ocPort #-}
{-# DEPRECATED port "Use generic-lens or generic-optics with 'port' instead"  #-}

-- | A list of VpcSecurityGroupMembership name strings used for this option.
--
-- /Note:/ Consider using 'vpcSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocVpcSecurityGroupMemberships :: Lens.Lens' OptionConfiguration (Core.Maybe [Core.Text])
ocVpcSecurityGroupMemberships = Lens.field @"vpcSecurityGroupMemberships"
{-# INLINEABLE ocVpcSecurityGroupMemberships #-}
{-# DEPRECATED vpcSecurityGroupMemberships "Use generic-lens or generic-optics with 'vpcSecurityGroupMemberships' instead"  #-}

instance Core.ToQuery OptionConfiguration where
        toQuery OptionConfiguration{..}
          = Core.toQueryPair "OptionName" optionName Core.<>
              Core.toQueryPair "DBSecurityGroupMemberships"
                (Core.maybe Core.mempty (Core.toQueryList "DBSecurityGroupName")
                   dBSecurityGroupMemberships)
              Core.<>
              Core.toQueryPair "OptionSettings"
                (Core.maybe Core.mempty (Core.toQueryList "OptionSetting")
                   optionSettings)
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "OptionVersion")
                optionVersion
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Port") port
              Core.<>
              Core.toQueryPair "VpcSecurityGroupMemberships"
                (Core.maybe Core.mempty (Core.toQueryList "VpcSecurityGroupId")
                   vpcSecurityGroupMemberships)
