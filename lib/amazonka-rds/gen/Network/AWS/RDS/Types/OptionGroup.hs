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
    ogOptionGroupDescription,
    ogVPCId,
    ogAllowsVPCAndNonVPCInstanceMemberships,
    ogEngineName,
    ogOptionGroupARN,
    ogMajorEngineVersion,
    ogOptions,
    ogOptionGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.Option

-- |
--
-- /See:/ 'mkOptionGroup' smart constructor.
data OptionGroup = OptionGroup'
  { optionGroupDescription ::
      Lude.Maybe Lude.Text,
    vpcId :: Lude.Maybe Lude.Text,
    allowsVPCAndNonVPCInstanceMemberships :: Lude.Maybe Lude.Bool,
    engineName :: Lude.Maybe Lude.Text,
    optionGroupARN :: Lude.Maybe Lude.Text,
    majorEngineVersion :: Lude.Maybe Lude.Text,
    options :: Lude.Maybe [Option],
    optionGroupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionGroup' with the minimum fields required to make a request.
--
-- * 'allowsVPCAndNonVPCInstanceMemberships' - Indicates whether this option group can be applied to both VPC and non-VPC instances. The value @true@ indicates the option group can be applied to both VPC and non-VPC instances.
-- * 'engineName' - Indicates the name of the engine that this option group can be applied to.
-- * 'majorEngineVersion' - Indicates the major engine version associated with this option group.
-- * 'optionGroupARN' - The Amazon Resource Name (ARN) for the option group.
-- * 'optionGroupDescription' - Provides a description of the option group.
-- * 'optionGroupName' - Specifies the name of the option group.
-- * 'options' - Indicates what options are available in the option group.
-- * 'vpcId' - If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@ , this field is blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this field is blank, then this option group can be applied to both VPC and non-VPC instances. If this field contains a value, then this option group can only be applied to instances that are in the VPC indicated by this field.
mkOptionGroup ::
  OptionGroup
mkOptionGroup =
  OptionGroup'
    { optionGroupDescription = Lude.Nothing,
      vpcId = Lude.Nothing,
      allowsVPCAndNonVPCInstanceMemberships = Lude.Nothing,
      engineName = Lude.Nothing,
      optionGroupARN = Lude.Nothing,
      majorEngineVersion = Lude.Nothing,
      options = Lude.Nothing,
      optionGroupName = Lude.Nothing
    }

-- | Provides a description of the option group.
--
-- /Note:/ Consider using 'optionGroupDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptionGroupDescription :: Lens.Lens' OptionGroup (Lude.Maybe Lude.Text)
ogOptionGroupDescription = Lens.lens (optionGroupDescription :: OptionGroup -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupDescription = a} :: OptionGroup)
{-# DEPRECATED ogOptionGroupDescription "Use generic-lens or generic-optics with 'optionGroupDescription' instead." #-}

-- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@ , this field is blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this field is blank, then this option group can be applied to both VPC and non-VPC instances. If this field contains a value, then this option group can only be applied to instances that are in the VPC indicated by this field.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogVPCId :: Lens.Lens' OptionGroup (Lude.Maybe Lude.Text)
ogVPCId = Lens.lens (vpcId :: OptionGroup -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: OptionGroup)
{-# DEPRECATED ogVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | Indicates whether this option group can be applied to both VPC and non-VPC instances. The value @true@ indicates the option group can be applied to both VPC and non-VPC instances.
--
-- /Note:/ Consider using 'allowsVPCAndNonVPCInstanceMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogAllowsVPCAndNonVPCInstanceMemberships :: Lens.Lens' OptionGroup (Lude.Maybe Lude.Bool)
ogAllowsVPCAndNonVPCInstanceMemberships = Lens.lens (allowsVPCAndNonVPCInstanceMemberships :: OptionGroup -> Lude.Maybe Lude.Bool) (\s a -> s {allowsVPCAndNonVPCInstanceMemberships = a} :: OptionGroup)
{-# DEPRECATED ogAllowsVPCAndNonVPCInstanceMemberships "Use generic-lens or generic-optics with 'allowsVPCAndNonVPCInstanceMemberships' instead." #-}

-- | Indicates the name of the engine that this option group can be applied to.
--
-- /Note:/ Consider using 'engineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogEngineName :: Lens.Lens' OptionGroup (Lude.Maybe Lude.Text)
ogEngineName = Lens.lens (engineName :: OptionGroup -> Lude.Maybe Lude.Text) (\s a -> s {engineName = a} :: OptionGroup)
{-# DEPRECATED ogEngineName "Use generic-lens or generic-optics with 'engineName' instead." #-}

-- | The Amazon Resource Name (ARN) for the option group.
--
-- /Note:/ Consider using 'optionGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptionGroupARN :: Lens.Lens' OptionGroup (Lude.Maybe Lude.Text)
ogOptionGroupARN = Lens.lens (optionGroupARN :: OptionGroup -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupARN = a} :: OptionGroup)
{-# DEPRECATED ogOptionGroupARN "Use generic-lens or generic-optics with 'optionGroupARN' instead." #-}

-- | Indicates the major engine version associated with this option group.
--
-- /Note:/ Consider using 'majorEngineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogMajorEngineVersion :: Lens.Lens' OptionGroup (Lude.Maybe Lude.Text)
ogMajorEngineVersion = Lens.lens (majorEngineVersion :: OptionGroup -> Lude.Maybe Lude.Text) (\s a -> s {majorEngineVersion = a} :: OptionGroup)
{-# DEPRECATED ogMajorEngineVersion "Use generic-lens or generic-optics with 'majorEngineVersion' instead." #-}

-- | Indicates what options are available in the option group.
--
-- /Note:/ Consider using 'options' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptions :: Lens.Lens' OptionGroup (Lude.Maybe [Option])
ogOptions = Lens.lens (options :: OptionGroup -> Lude.Maybe [Option]) (\s a -> s {options = a} :: OptionGroup)
{-# DEPRECATED ogOptions "Use generic-lens or generic-optics with 'options' instead." #-}

-- | Specifies the name of the option group.
--
-- /Note:/ Consider using 'optionGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogOptionGroupName :: Lens.Lens' OptionGroup (Lude.Maybe Lude.Text)
ogOptionGroupName = Lens.lens (optionGroupName :: OptionGroup -> Lude.Maybe Lude.Text) (\s a -> s {optionGroupName = a} :: OptionGroup)
{-# DEPRECATED ogOptionGroupName "Use generic-lens or generic-optics with 'optionGroupName' instead." #-}

instance Lude.FromXML OptionGroup where
  parseXML x =
    OptionGroup'
      Lude.<$> (x Lude..@? "OptionGroupDescription")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> (x Lude..@? "AllowsVpcAndNonVpcInstanceMemberships")
      Lude.<*> (x Lude..@? "EngineName")
      Lude.<*> (x Lude..@? "OptionGroupArn")
      Lude.<*> (x Lude..@? "MajorEngineVersion")
      Lude.<*> ( x Lude..@? "Options" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "Option")
               )
      Lude.<*> (x Lude..@? "OptionGroupName")
