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
-- Module      : Network.AWS.RDS.Types.OptionGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionGroup where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.Option

-- |
--
-- /See:/ 'newOptionGroup' smart constructor.
data OptionGroup = OptionGroup'
  { -- | Indicates the name of the engine that this option group can be applied
    -- to.
    engineName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) for the option group.
    optionGroupArn :: Core.Maybe Core.Text,
    -- | Indicates whether this option group can be applied to both VPC and
    -- non-VPC instances. The value @true@ indicates the option group can be
    -- applied to both VPC and non-VPC instances.
    allowsVpcAndNonVpcInstanceMemberships :: Core.Maybe Core.Bool,
    -- | Specifies the name of the option group.
    optionGroupName :: Core.Maybe Core.Text,
    -- | Indicates what options are available in the option group.
    options :: Core.Maybe [Option],
    -- | Provides a description of the option group.
    optionGroupDescription :: Core.Maybe Core.Text,
    -- | Indicates the major engine version associated with this option group.
    majorEngineVersion :: Core.Maybe Core.Text,
    -- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@, this field is
    -- blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this
    -- field is blank, then this option group can be applied to both VPC and
    -- non-VPC instances. If this field contains a value, then this option
    -- group can only be applied to instances that are in the VPC indicated by
    -- this field.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OptionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineName', 'optionGroup_engineName' - Indicates the name of the engine that this option group can be applied
-- to.
--
-- 'optionGroupArn', 'optionGroup_optionGroupArn' - The Amazon Resource Name (ARN) for the option group.
--
-- 'allowsVpcAndNonVpcInstanceMemberships', 'optionGroup_allowsVpcAndNonVpcInstanceMemberships' - Indicates whether this option group can be applied to both VPC and
-- non-VPC instances. The value @true@ indicates the option group can be
-- applied to both VPC and non-VPC instances.
--
-- 'optionGroupName', 'optionGroup_optionGroupName' - Specifies the name of the option group.
--
-- 'options', 'optionGroup_options' - Indicates what options are available in the option group.
--
-- 'optionGroupDescription', 'optionGroup_optionGroupDescription' - Provides a description of the option group.
--
-- 'majorEngineVersion', 'optionGroup_majorEngineVersion' - Indicates the major engine version associated with this option group.
--
-- 'vpcId', 'optionGroup_vpcId' - If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@, this field is
-- blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this
-- field is blank, then this option group can be applied to both VPC and
-- non-VPC instances. If this field contains a value, then this option
-- group can only be applied to instances that are in the VPC indicated by
-- this field.
newOptionGroup ::
  OptionGroup
newOptionGroup =
  OptionGroup'
    { engineName = Core.Nothing,
      optionGroupArn = Core.Nothing,
      allowsVpcAndNonVpcInstanceMemberships = Core.Nothing,
      optionGroupName = Core.Nothing,
      options = Core.Nothing,
      optionGroupDescription = Core.Nothing,
      majorEngineVersion = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | Indicates the name of the engine that this option group can be applied
-- to.
optionGroup_engineName :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
optionGroup_engineName = Lens.lens (\OptionGroup' {engineName} -> engineName) (\s@OptionGroup' {} a -> s {engineName = a} :: OptionGroup)

-- | The Amazon Resource Name (ARN) for the option group.
optionGroup_optionGroupArn :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
optionGroup_optionGroupArn = Lens.lens (\OptionGroup' {optionGroupArn} -> optionGroupArn) (\s@OptionGroup' {} a -> s {optionGroupArn = a} :: OptionGroup)

-- | Indicates whether this option group can be applied to both VPC and
-- non-VPC instances. The value @true@ indicates the option group can be
-- applied to both VPC and non-VPC instances.
optionGroup_allowsVpcAndNonVpcInstanceMemberships :: Lens.Lens' OptionGroup (Core.Maybe Core.Bool)
optionGroup_allowsVpcAndNonVpcInstanceMemberships = Lens.lens (\OptionGroup' {allowsVpcAndNonVpcInstanceMemberships} -> allowsVpcAndNonVpcInstanceMemberships) (\s@OptionGroup' {} a -> s {allowsVpcAndNonVpcInstanceMemberships = a} :: OptionGroup)

-- | Specifies the name of the option group.
optionGroup_optionGroupName :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
optionGroup_optionGroupName = Lens.lens (\OptionGroup' {optionGroupName} -> optionGroupName) (\s@OptionGroup' {} a -> s {optionGroupName = a} :: OptionGroup)

-- | Indicates what options are available in the option group.
optionGroup_options :: Lens.Lens' OptionGroup (Core.Maybe [Option])
optionGroup_options = Lens.lens (\OptionGroup' {options} -> options) (\s@OptionGroup' {} a -> s {options = a} :: OptionGroup) Core.. Lens.mapping Lens._Coerce

-- | Provides a description of the option group.
optionGroup_optionGroupDescription :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
optionGroup_optionGroupDescription = Lens.lens (\OptionGroup' {optionGroupDescription} -> optionGroupDescription) (\s@OptionGroup' {} a -> s {optionGroupDescription = a} :: OptionGroup)

-- | Indicates the major engine version associated with this option group.
optionGroup_majorEngineVersion :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
optionGroup_majorEngineVersion = Lens.lens (\OptionGroup' {majorEngineVersion} -> majorEngineVersion) (\s@OptionGroup' {} a -> s {majorEngineVersion = a} :: OptionGroup)

-- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@, this field is
-- blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this
-- field is blank, then this option group can be applied to both VPC and
-- non-VPC instances. If this field contains a value, then this option
-- group can only be applied to instances that are in the VPC indicated by
-- this field.
optionGroup_vpcId :: Lens.Lens' OptionGroup (Core.Maybe Core.Text)
optionGroup_vpcId = Lens.lens (\OptionGroup' {vpcId} -> vpcId) (\s@OptionGroup' {} a -> s {vpcId = a} :: OptionGroup)

instance Core.FromXML OptionGroup where
  parseXML x =
    OptionGroup'
      Core.<$> (x Core..@? "EngineName")
      Core.<*> (x Core..@? "OptionGroupArn")
      Core.<*> (x Core..@? "AllowsVpcAndNonVpcInstanceMemberships")
      Core.<*> (x Core..@? "OptionGroupName")
      Core.<*> ( x Core..@? "Options" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "Option")
               )
      Core.<*> (x Core..@? "OptionGroupDescription")
      Core.<*> (x Core..@? "MajorEngineVersion")
      Core.<*> (x Core..@? "VpcId")

instance Core.Hashable OptionGroup

instance Core.NFData OptionGroup
