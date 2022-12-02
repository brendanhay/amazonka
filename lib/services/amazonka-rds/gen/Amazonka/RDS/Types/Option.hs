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
-- Module      : Amazonka.RDS.Types.Option
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.Option where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.DBSecurityGroupMembership
import Amazonka.RDS.Types.OptionSetting
import Amazonka.RDS.Types.VpcSecurityGroupMembership

-- | Option details.
--
-- /See:/ 'newOption' smart constructor.
data Option = Option'
  { -- | If required, the port configured for this option to use.
    port :: Prelude.Maybe Prelude.Int,
    -- | If the option requires access to a port, then this DB security group
    -- allows access to the port.
    dbSecurityGroupMemberships :: Prelude.Maybe [DBSecurityGroupMembership],
    -- | The version of the option.
    optionVersion :: Prelude.Maybe Prelude.Text,
    -- | Indicate if this option is persistent.
    persistent :: Prelude.Maybe Prelude.Bool,
    -- | The name of the option.
    optionName :: Prelude.Maybe Prelude.Text,
    -- | The description of the option.
    optionDescription :: Prelude.Maybe Prelude.Text,
    -- | Indicate if this option is permanent.
    permanent :: Prelude.Maybe Prelude.Bool,
    -- | If the option requires access to a port, then this VPC security group
    -- allows access to the port.
    vpcSecurityGroupMemberships :: Prelude.Maybe [VpcSecurityGroupMembership],
    -- | The option settings for this option.
    optionSettings :: Prelude.Maybe [OptionSetting]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Option' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'option_port' - If required, the port configured for this option to use.
--
-- 'dbSecurityGroupMemberships', 'option_dbSecurityGroupMemberships' - If the option requires access to a port, then this DB security group
-- allows access to the port.
--
-- 'optionVersion', 'option_optionVersion' - The version of the option.
--
-- 'persistent', 'option_persistent' - Indicate if this option is persistent.
--
-- 'optionName', 'option_optionName' - The name of the option.
--
-- 'optionDescription', 'option_optionDescription' - The description of the option.
--
-- 'permanent', 'option_permanent' - Indicate if this option is permanent.
--
-- 'vpcSecurityGroupMemberships', 'option_vpcSecurityGroupMemberships' - If the option requires access to a port, then this VPC security group
-- allows access to the port.
--
-- 'optionSettings', 'option_optionSettings' - The option settings for this option.
newOption ::
  Option
newOption =
  Option'
    { port = Prelude.Nothing,
      dbSecurityGroupMemberships = Prelude.Nothing,
      optionVersion = Prelude.Nothing,
      persistent = Prelude.Nothing,
      optionName = Prelude.Nothing,
      optionDescription = Prelude.Nothing,
      permanent = Prelude.Nothing,
      vpcSecurityGroupMemberships = Prelude.Nothing,
      optionSettings = Prelude.Nothing
    }

-- | If required, the port configured for this option to use.
option_port :: Lens.Lens' Option (Prelude.Maybe Prelude.Int)
option_port = Lens.lens (\Option' {port} -> port) (\s@Option' {} a -> s {port = a} :: Option)

-- | If the option requires access to a port, then this DB security group
-- allows access to the port.
option_dbSecurityGroupMemberships :: Lens.Lens' Option (Prelude.Maybe [DBSecurityGroupMembership])
option_dbSecurityGroupMemberships = Lens.lens (\Option' {dbSecurityGroupMemberships} -> dbSecurityGroupMemberships) (\s@Option' {} a -> s {dbSecurityGroupMemberships = a} :: Option) Prelude.. Lens.mapping Lens.coerced

-- | The version of the option.
option_optionVersion :: Lens.Lens' Option (Prelude.Maybe Prelude.Text)
option_optionVersion = Lens.lens (\Option' {optionVersion} -> optionVersion) (\s@Option' {} a -> s {optionVersion = a} :: Option)

-- | Indicate if this option is persistent.
option_persistent :: Lens.Lens' Option (Prelude.Maybe Prelude.Bool)
option_persistent = Lens.lens (\Option' {persistent} -> persistent) (\s@Option' {} a -> s {persistent = a} :: Option)

-- | The name of the option.
option_optionName :: Lens.Lens' Option (Prelude.Maybe Prelude.Text)
option_optionName = Lens.lens (\Option' {optionName} -> optionName) (\s@Option' {} a -> s {optionName = a} :: Option)

-- | The description of the option.
option_optionDescription :: Lens.Lens' Option (Prelude.Maybe Prelude.Text)
option_optionDescription = Lens.lens (\Option' {optionDescription} -> optionDescription) (\s@Option' {} a -> s {optionDescription = a} :: Option)

-- | Indicate if this option is permanent.
option_permanent :: Lens.Lens' Option (Prelude.Maybe Prelude.Bool)
option_permanent = Lens.lens (\Option' {permanent} -> permanent) (\s@Option' {} a -> s {permanent = a} :: Option)

-- | If the option requires access to a port, then this VPC security group
-- allows access to the port.
option_vpcSecurityGroupMemberships :: Lens.Lens' Option (Prelude.Maybe [VpcSecurityGroupMembership])
option_vpcSecurityGroupMemberships = Lens.lens (\Option' {vpcSecurityGroupMemberships} -> vpcSecurityGroupMemberships) (\s@Option' {} a -> s {vpcSecurityGroupMemberships = a} :: Option) Prelude.. Lens.mapping Lens.coerced

-- | The option settings for this option.
option_optionSettings :: Lens.Lens' Option (Prelude.Maybe [OptionSetting])
option_optionSettings = Lens.lens (\Option' {optionSettings} -> optionSettings) (\s@Option' {} a -> s {optionSettings = a} :: Option) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML Option where
  parseXML x =
    Option'
      Prelude.<$> (x Data..@? "Port")
      Prelude.<*> ( x Data..@? "DBSecurityGroupMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "DBSecurityGroup")
                  )
      Prelude.<*> (x Data..@? "OptionVersion")
      Prelude.<*> (x Data..@? "Persistent")
      Prelude.<*> (x Data..@? "OptionName")
      Prelude.<*> (x Data..@? "OptionDescription")
      Prelude.<*> (x Data..@? "Permanent")
      Prelude.<*> ( x Data..@? "VpcSecurityGroupMemberships"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "VpcSecurityGroupMembership")
                  )
      Prelude.<*> ( x Data..@? "OptionSettings" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "OptionSetting")
                  )

instance Prelude.Hashable Option where
  hashWithSalt _salt Option' {..} =
    _salt `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` dbSecurityGroupMemberships
      `Prelude.hashWithSalt` optionVersion
      `Prelude.hashWithSalt` persistent
      `Prelude.hashWithSalt` optionName
      `Prelude.hashWithSalt` optionDescription
      `Prelude.hashWithSalt` permanent
      `Prelude.hashWithSalt` vpcSecurityGroupMemberships
      `Prelude.hashWithSalt` optionSettings

instance Prelude.NFData Option where
  rnf Option' {..} =
    Prelude.rnf port
      `Prelude.seq` Prelude.rnf dbSecurityGroupMemberships
      `Prelude.seq` Prelude.rnf optionVersion
      `Prelude.seq` Prelude.rnf persistent
      `Prelude.seq` Prelude.rnf optionName
      `Prelude.seq` Prelude.rnf optionDescription
      `Prelude.seq` Prelude.rnf permanent
      `Prelude.seq` Prelude.rnf vpcSecurityGroupMemberships
      `Prelude.seq` Prelude.rnf optionSettings
