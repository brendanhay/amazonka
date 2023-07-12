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
-- Module      : Amazonka.RDS.Types.OptionGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.OptionGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.Option

-- |
--
-- /See:/ 'newOptionGroup' smart constructor.
data OptionGroup = OptionGroup'
  { -- | Indicates whether this option group can be applied to both VPC and
    -- non-VPC instances. The value @true@ indicates the option group can be
    -- applied to both VPC and non-VPC instances.
    allowsVpcAndNonVpcInstanceMemberships :: Prelude.Maybe Prelude.Bool,
    -- | Indicates when the option group was copied.
    copyTimestamp :: Prelude.Maybe Data.ISO8601,
    -- | Indicates the name of the engine that this option group can be applied
    -- to.
    engineName :: Prelude.Maybe Prelude.Text,
    -- | Indicates the major engine version associated with this option group.
    majorEngineVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon Resource Name (ARN) for the option group.
    optionGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Provides a description of the option group.
    optionGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the option group.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | Indicates what options are available in the option group.
    options :: Prelude.Maybe [Option],
    -- | Specifies the Amazon Web Services account ID for the option group from
    -- which this option group is copied.
    sourceAccountId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of the option group from which this option group is
    -- copied.
    sourceOptionGroup :: Prelude.Maybe Prelude.Text,
    -- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@, this field is
    -- blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this
    -- field is blank, then this option group can be applied to both VPC and
    -- non-VPC instances. If this field contains a value, then this option
    -- group can only be applied to instances that are in the VPC indicated by
    -- this field.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptionGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowsVpcAndNonVpcInstanceMemberships', 'optionGroup_allowsVpcAndNonVpcInstanceMemberships' - Indicates whether this option group can be applied to both VPC and
-- non-VPC instances. The value @true@ indicates the option group can be
-- applied to both VPC and non-VPC instances.
--
-- 'copyTimestamp', 'optionGroup_copyTimestamp' - Indicates when the option group was copied.
--
-- 'engineName', 'optionGroup_engineName' - Indicates the name of the engine that this option group can be applied
-- to.
--
-- 'majorEngineVersion', 'optionGroup_majorEngineVersion' - Indicates the major engine version associated with this option group.
--
-- 'optionGroupArn', 'optionGroup_optionGroupArn' - Specifies the Amazon Resource Name (ARN) for the option group.
--
-- 'optionGroupDescription', 'optionGroup_optionGroupDescription' - Provides a description of the option group.
--
-- 'optionGroupName', 'optionGroup_optionGroupName' - Specifies the name of the option group.
--
-- 'options', 'optionGroup_options' - Indicates what options are available in the option group.
--
-- 'sourceAccountId', 'optionGroup_sourceAccountId' - Specifies the Amazon Web Services account ID for the option group from
-- which this option group is copied.
--
-- 'sourceOptionGroup', 'optionGroup_sourceOptionGroup' - Specifies the name of the option group from which this option group is
-- copied.
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
    { allowsVpcAndNonVpcInstanceMemberships =
        Prelude.Nothing,
      copyTimestamp = Prelude.Nothing,
      engineName = Prelude.Nothing,
      majorEngineVersion = Prelude.Nothing,
      optionGroupArn = Prelude.Nothing,
      optionGroupDescription = Prelude.Nothing,
      optionGroupName = Prelude.Nothing,
      options = Prelude.Nothing,
      sourceAccountId = Prelude.Nothing,
      sourceOptionGroup = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | Indicates whether this option group can be applied to both VPC and
-- non-VPC instances. The value @true@ indicates the option group can be
-- applied to both VPC and non-VPC instances.
optionGroup_allowsVpcAndNonVpcInstanceMemberships :: Lens.Lens' OptionGroup (Prelude.Maybe Prelude.Bool)
optionGroup_allowsVpcAndNonVpcInstanceMemberships = Lens.lens (\OptionGroup' {allowsVpcAndNonVpcInstanceMemberships} -> allowsVpcAndNonVpcInstanceMemberships) (\s@OptionGroup' {} a -> s {allowsVpcAndNonVpcInstanceMemberships = a} :: OptionGroup)

-- | Indicates when the option group was copied.
optionGroup_copyTimestamp :: Lens.Lens' OptionGroup (Prelude.Maybe Prelude.UTCTime)
optionGroup_copyTimestamp = Lens.lens (\OptionGroup' {copyTimestamp} -> copyTimestamp) (\s@OptionGroup' {} a -> s {copyTimestamp = a} :: OptionGroup) Prelude.. Lens.mapping Data._Time

-- | Indicates the name of the engine that this option group can be applied
-- to.
optionGroup_engineName :: Lens.Lens' OptionGroup (Prelude.Maybe Prelude.Text)
optionGroup_engineName = Lens.lens (\OptionGroup' {engineName} -> engineName) (\s@OptionGroup' {} a -> s {engineName = a} :: OptionGroup)

-- | Indicates the major engine version associated with this option group.
optionGroup_majorEngineVersion :: Lens.Lens' OptionGroup (Prelude.Maybe Prelude.Text)
optionGroup_majorEngineVersion = Lens.lens (\OptionGroup' {majorEngineVersion} -> majorEngineVersion) (\s@OptionGroup' {} a -> s {majorEngineVersion = a} :: OptionGroup)

-- | Specifies the Amazon Resource Name (ARN) for the option group.
optionGroup_optionGroupArn :: Lens.Lens' OptionGroup (Prelude.Maybe Prelude.Text)
optionGroup_optionGroupArn = Lens.lens (\OptionGroup' {optionGroupArn} -> optionGroupArn) (\s@OptionGroup' {} a -> s {optionGroupArn = a} :: OptionGroup)

-- | Provides a description of the option group.
optionGroup_optionGroupDescription :: Lens.Lens' OptionGroup (Prelude.Maybe Prelude.Text)
optionGroup_optionGroupDescription = Lens.lens (\OptionGroup' {optionGroupDescription} -> optionGroupDescription) (\s@OptionGroup' {} a -> s {optionGroupDescription = a} :: OptionGroup)

-- | Specifies the name of the option group.
optionGroup_optionGroupName :: Lens.Lens' OptionGroup (Prelude.Maybe Prelude.Text)
optionGroup_optionGroupName = Lens.lens (\OptionGroup' {optionGroupName} -> optionGroupName) (\s@OptionGroup' {} a -> s {optionGroupName = a} :: OptionGroup)

-- | Indicates what options are available in the option group.
optionGroup_options :: Lens.Lens' OptionGroup (Prelude.Maybe [Option])
optionGroup_options = Lens.lens (\OptionGroup' {options} -> options) (\s@OptionGroup' {} a -> s {options = a} :: OptionGroup) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the Amazon Web Services account ID for the option group from
-- which this option group is copied.
optionGroup_sourceAccountId :: Lens.Lens' OptionGroup (Prelude.Maybe Prelude.Text)
optionGroup_sourceAccountId = Lens.lens (\OptionGroup' {sourceAccountId} -> sourceAccountId) (\s@OptionGroup' {} a -> s {sourceAccountId = a} :: OptionGroup)

-- | Specifies the name of the option group from which this option group is
-- copied.
optionGroup_sourceOptionGroup :: Lens.Lens' OptionGroup (Prelude.Maybe Prelude.Text)
optionGroup_sourceOptionGroup = Lens.lens (\OptionGroup' {sourceOptionGroup} -> sourceOptionGroup) (\s@OptionGroup' {} a -> s {sourceOptionGroup = a} :: OptionGroup)

-- | If __AllowsVpcAndNonVpcInstanceMemberships__ is @false@, this field is
-- blank. If __AllowsVpcAndNonVpcInstanceMemberships__ is @true@ and this
-- field is blank, then this option group can be applied to both VPC and
-- non-VPC instances. If this field contains a value, then this option
-- group can only be applied to instances that are in the VPC indicated by
-- this field.
optionGroup_vpcId :: Lens.Lens' OptionGroup (Prelude.Maybe Prelude.Text)
optionGroup_vpcId = Lens.lens (\OptionGroup' {vpcId} -> vpcId) (\s@OptionGroup' {} a -> s {vpcId = a} :: OptionGroup)

instance Data.FromXML OptionGroup where
  parseXML x =
    OptionGroup'
      Prelude.<$> (x Data..@? "AllowsVpcAndNonVpcInstanceMemberships")
      Prelude.<*> (x Data..@? "CopyTimestamp")
      Prelude.<*> (x Data..@? "EngineName")
      Prelude.<*> (x Data..@? "MajorEngineVersion")
      Prelude.<*> (x Data..@? "OptionGroupArn")
      Prelude.<*> (x Data..@? "OptionGroupDescription")
      Prelude.<*> (x Data..@? "OptionGroupName")
      Prelude.<*> ( x
                      Data..@? "Options"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "Option")
                  )
      Prelude.<*> (x Data..@? "SourceAccountId")
      Prelude.<*> (x Data..@? "SourceOptionGroup")
      Prelude.<*> (x Data..@? "VpcId")

instance Prelude.Hashable OptionGroup where
  hashWithSalt _salt OptionGroup' {..} =
    _salt
      `Prelude.hashWithSalt` allowsVpcAndNonVpcInstanceMemberships
      `Prelude.hashWithSalt` copyTimestamp
      `Prelude.hashWithSalt` engineName
      `Prelude.hashWithSalt` majorEngineVersion
      `Prelude.hashWithSalt` optionGroupArn
      `Prelude.hashWithSalt` optionGroupDescription
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` sourceAccountId
      `Prelude.hashWithSalt` sourceOptionGroup
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData OptionGroup where
  rnf OptionGroup' {..} =
    Prelude.rnf allowsVpcAndNonVpcInstanceMemberships
      `Prelude.seq` Prelude.rnf copyTimestamp
      `Prelude.seq` Prelude.rnf engineName
      `Prelude.seq` Prelude.rnf majorEngineVersion
      `Prelude.seq` Prelude.rnf optionGroupArn
      `Prelude.seq` Prelude.rnf optionGroupDescription
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf sourceAccountId
      `Prelude.seq` Prelude.rnf sourceOptionGroup
      `Prelude.seq` Prelude.rnf vpcId
