{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.OptionGroup
  ( OptionGroup (..)
  -- * Smart constructor
  , mkOptionGroup
  -- * Lenses
  , ogAllowsVpcAndNonVpcInstanceMemberships
  , ogEngineName
  , ogMajorEngineVersion
  , ogOptionGroupArn
  , ogOptionGroupDescription
  , ogOptionGroupName
  , ogOptions
  , ogVpcId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.Option as Types

-- | 
--
-- /See:/ 'mkOptionGroup' smart constructor.
data OptionGroup = OptionGroup'
  { allowsVpcAndNonVpcInstanceMemberships :: Core.Maybe Core.Bool
    -- ^ Indicates whether this option group can be applied to both VPC and non-VPC instances. The value @true@ indicates the option group can be applied to both VPC and non-VPC instances. 
  , engineName :: Core.Maybe Core.Text
    -- ^ Indicates the name of the engine that this option group can be applied to.
  , majorEngineVersion :: Core.Maybe Core.Text
    -- ^ Indicates the major engine version associated with this option group.
  , optionGroupArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the option group.
  , optionGroupDescription :: Core.Maybe Core.Text
    -- ^ Provides a description of the option group.
  , optionGroupName :: Core.Maybe Core.Text
    -- ^ Specifies the name of the option group.
  , options :: Core.Maybe [Types.Option]
    -- ^ Indicates what options are available in the option group.
  , vpcId :: Core.Maybe Core.Text
    -- ^ If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@ , this field is blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this field is blank, then this option group can be applied to both VPC and non-VPC instances. If this field contains a value, then this option group can only be applied to instances that are in the VPC indicated by this field. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OptionGroup' value with any optional fields omitted.
mkOptionGroup
    :: OptionGroup
mkOptionGroup
  = OptionGroup'{allowsVpcAndNonVpcInstanceMemberships =
                   Core.Nothing,
                 engineName = Core.Nothing, majorEngineVersion = Core.Nothing,
                 optionGroupArn = Core.Nothing,
                 optionGroupDescription = Core.Nothing,
                 optionGroupName = Core.Nothing, options = Core.Nothing,
                 vpcId = Core.Nothing}

-- | Indicates whether this option group can be applied to both VPC and non-VPC instances. The value @true@ indicates the option group can be applied to both VPC and non-VPC instances. 
--
-- /Note:/ Consider using 'allowsVpcAndNonVpcInstanceMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogAllowsVpcAndNonVpcInstanceMemberships :: Lens.Lens' OptionGroup (Core.Maybe Core.Bool)
ogAllowsVpcAndNonVpcInstanceMemberships = Lens.field @"allowsVpcAndNonVpcInstanceMemberships"
{-# INLINEABLE ogAllowsVpcAndNonVpcInstanceMemberships #-}
{-# DEPRECATED allowsVpcAndNonVpcInstanceMemberships "Use generic-lens or generic-optics with 'allowsVpcAndNonVpcInstanceMemberships' instead"  #-}

-- | Indicates the name of the engine that this option group can be applied to.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogEngineName :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
ogEngineName = Lens.field @"engineName"
{-# INLINEABLE ogEngineName #-}
{-# DEPRECATED engineName "Use generic-lens or generic-optics with 'engineName' instead"  #-}

-- | Indicates the major engine version associated with this option group.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogMajorEngineVersion :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
ogMajorEngineVersion = Lens.field @"majorEngineVersion"
{-# INLINEABLE ogMajorEngineVersion #-}
{-# DEPRECATED majorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead"  #-}

-- | The Amazon Resource Name (ARN) for the option group.
--
-- /Note:/ Consider using 'optionGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptionGroupArn :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
ogOptionGroupArn = Lens.field @"optionGroupArn"
{-# INLINEABLE ogOptionGroupArn #-}
{-# DEPRECATED optionGroupArn "Use generic-lens or generic-optics with 'optionGroupArn' instead"  #-}

-- | Provides a description of the option group.
--
-- /Note:/ Consider using 'optionGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptionGroupDescription :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
ogOptionGroupDescription = Lens.field @"optionGroupDescription"
{-# INLINEABLE ogOptionGroupDescription #-}
{-# DEPRECATED optionGroupDescription "Use generic-lens or generic-optics with 'optionGroupDescription' instead"  #-}

-- | Specifies the name of the option group.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptionGroupName :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
ogOptionGroupName = Lens.field @"optionGroupName"
{-# INLINEABLE ogOptionGroupName #-}
{-# DEPRECATED optionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead"  #-}

-- | Indicates what options are available in the option group.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptions :: Lens.Lens' OptionGroup (Core.Maybe [Types.Option])
ogOptions = Lens.field @"options"
{-# INLINEABLE ogOptions #-}
{-# DEPRECATED options "Use generic-lens or generic-optics with 'options' instead"  #-}

-- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@ , this field is blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this field is blank, then this option group can be applied to both VPC and non-VPC instances. If this field contains a value, then this option group can only be applied to instances that are in the VPC indicated by this field. 
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogVpcId :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
ogVpcId = Lens.field @"vpcId"
{-# INLINEABLE ogVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML OptionGroup where
        parseXML x
          = OptionGroup' Core.<$>
              (x Core..@? "AllowsVpcAndNonVpcInstanceMemberships") Core.<*>
                x Core..@? "EngineName"
                Core.<*> x Core..@? "MajorEngineVersion"
                Core.<*> x Core..@? "OptionGroupArn"
                Core.<*> x Core..@? "OptionGroupDescription"
                Core.<*> x Core..@? "OptionGroupName"
                Core.<*> x Core..@? "Options" Core..<@> Core.parseXMLList "Option"
                Core.<*> x Core..@? "VpcId"
