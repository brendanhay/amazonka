{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionGroup
  ( OptionGroup (..),

    -- * Smart constructor
    mkOptionGroup,

    -- * Lenses
    ogAllowsVpcAndNonVpcInstanceMemberships,
    ogEngineName,
    ogMajorEngineVersion,
    ogOptionGroupArn,
    ogOptionGroupDescription,
    ogOptionGroupName,
    ogOptions,
    ogVpcId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.EngineName as Types
import qualified Network.AWS.RDS.Types.MajorEngineVersion as Types
import qualified Network.AWS.RDS.Types.Option as Types
import qualified Network.AWS.RDS.Types.OptionGroupArn as Types
import qualified Network.AWS.RDS.Types.OptionGroupDescription as Types
import qualified Network.AWS.RDS.Types.OptionGroupName as Types
import qualified Network.AWS.RDS.Types.VpcId as Types

-- |
--
-- /See:/ 'mkOptionGroup' smart constructor.
data OptionGroup = OptionGroup'
  { -- | Indicates whether this option group can be applied to both VPC and non-VPC instances. The value @true@ indicates the option group can be applied to both VPC and non-VPC instances.
    allowsVpcAndNonVpcInstanceMemberships :: Core.Maybe Core.Bool,
    -- | Indicates the name of the engine that this option group can be applied to.
    engineName :: Core.Maybe Types.EngineName,
    -- | Indicates the major engine version associated with this option group.
    majorEngineVersion :: Core.Maybe Types.MajorEngineVersion,
    -- | The Amazon Resource Name (ARN) for the option group.
    optionGroupArn :: Core.Maybe Types.OptionGroupArn,
    -- | Provides a description of the option group.
    optionGroupDescription :: Core.Maybe Types.OptionGroupDescription,
    -- | Specifies the name of the option group.
    optionGroupName :: Core.Maybe Types.OptionGroupName,
    -- | Indicates what options are available in the option group.
    options :: Core.Maybe [Types.Option],
    -- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@ , this field is blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this field is blank, then this option group can be applied to both VPC and non-VPC instances. If this field contains a value, then this option group can only be applied to instances that are in the VPC indicated by this field.
    vpcId :: Core.Maybe Types.VpcId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionGroup' value with any optional fields omitted.
mkOptionGroup ::
  OptionGroup
mkOptionGroup =
  OptionGroup'
    { allowsVpcAndNonVpcInstanceMemberships =
        Core.Nothing,
      engineName = Core.Nothing,
      majorEngineVersion = Core.Nothing,
      optionGroupArn = Core.Nothing,
      optionGroupDescription = Core.Nothing,
      optionGroupName = Core.Nothing,
      options = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | Indicates whether this option group can be applied to both VPC and non-VPC instances. The value @true@ indicates the option group can be applied to both VPC and non-VPC instances.
--
-- /Note:/ Consider using 'allowsVpcAndNonVpcInstanceMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogAllowsVpcAndNonVpcInstanceMemberships :: Lens.Lens' OptionGroup (Core.Maybe Core.Bool)
ogAllowsVpcAndNonVpcInstanceMemberships = Lens.field @"allowsVpcAndNonVpcInstanceMemberships"
{-# DEPRECATED ogAllowsVpcAndNonVpcInstanceMemberships "Use generic-lens or generic-optics with 'allowsVpcAndNonVpcInstanceMemberships' instead." #-}

-- | Indicates the name of the engine that this option group can be applied to.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogEngineName :: Lens.Lens' OptionGroup (Core.Maybe Types.EngineName)
ogEngineName = Lens.field @"engineName"
{-# DEPRECATED ogEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | Indicates the major engine version associated with this option group.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogMajorEngineVersion :: Lens.Lens' OptionGroup (Core.Maybe Types.MajorEngineVersion)
ogMajorEngineVersion = Lens.field @"majorEngineVersion"
{-# DEPRECATED ogMajorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead." #-}

-- | The Amazon Resource Name (ARN) for the option group.
--
-- /Note:/ Consider using 'optionGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptionGroupArn :: Lens.Lens' OptionGroup (Core.Maybe Types.OptionGroupArn)
ogOptionGroupArn = Lens.field @"optionGroupArn"
{-# DEPRECATED ogOptionGroupArn "Use generic-lens or generic-optics with 'optionGroupArn' instead." #-}

-- | Provides a description of the option group.
--
-- /Note:/ Consider using 'optionGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptionGroupDescription :: Lens.Lens' OptionGroup (Core.Maybe Types.OptionGroupDescription)
ogOptionGroupDescription = Lens.field @"optionGroupDescription"
{-# DEPRECATED ogOptionGroupDescription "Use generic-lens or generic-optics with 'optionGroupDescription' instead." #-}

-- | Specifies the name of the option group.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptionGroupName :: Lens.Lens' OptionGroup (Core.Maybe Types.OptionGroupName)
ogOptionGroupName = Lens.field @"optionGroupName"
{-# DEPRECATED ogOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

-- | Indicates what options are available in the option group.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptions :: Lens.Lens' OptionGroup (Core.Maybe [Types.Option])
ogOptions = Lens.field @"options"
{-# DEPRECATED ogOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@ , this field is blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this field is blank, then this option group can be applied to both VPC and non-VPC instances. If this field contains a value, then this option group can only be applied to instances that are in the VPC indicated by this field.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogVpcId :: Lens.Lens' OptionGroup (Core.Maybe Types.VpcId)
ogVpcId = Lens.field @"vpcId"
{-# DEPRECATED ogVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML OptionGroup where
  parseXML x =
    OptionGroup'
      Core.<$> (x Core..@? "AllowsVpcAndNonVpcInstanceMemberships")
      Core.<*> (x Core..@? "EngineName")
      Core.<*> (x Core..@? "MajorEngineVersion")
      Core.<*> (x Core..@? "OptionGroupArn")
      Core.<*> (x Core..@? "OptionGroupDescription")
      Core.<*> (x Core..@? "OptionGroupName")
      Core.<*> (x Core..@? "Options" Core..<@> Core.parseXMLList "Option")
      Core.<*> (x Core..@? "VpcId")
