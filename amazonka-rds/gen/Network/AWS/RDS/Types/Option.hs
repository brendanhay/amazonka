{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.RDS.Types.Option
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Option where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.RDS.Types.DBSecurityGroupMembership
import Network.AWS.RDS.Types.OptionSetting
import Network.AWS.RDS.Types.VpcSecurityGroupMembership

-- | Option details.
--
-- /See:/ 'newOption' smart constructor.
data Option = Option'
  { -- | The name of the option.
    optionName :: Prelude.Maybe Prelude.Text,
    -- | The version of the option.
    optionVersion :: Prelude.Maybe Prelude.Text,
    -- | If the option requires access to a port, then this DB security group
    -- allows access to the port.
    dbSecurityGroupMemberships :: Prelude.Maybe [DBSecurityGroupMembership],
    -- | The description of the option.
    optionDescription :: Prelude.Maybe Prelude.Text,
    -- | If required, the port configured for this option to use.
    port :: Prelude.Maybe Prelude.Int,
    -- | The option settings for this option.
    optionSettings :: Prelude.Maybe [OptionSetting],
    -- | Indicate if this option is persistent.
    persistent :: Prelude.Maybe Prelude.Bool,
    -- | If the option requires access to a port, then this VPC security group
    -- allows access to the port.
    vpcSecurityGroupMemberships :: Prelude.Maybe [VpcSecurityGroupMembership],
    -- | Indicate if this option is permanent.
    permanent :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Option' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionName', 'option_optionName' - The name of the option.
--
-- 'optionVersion', 'option_optionVersion' - The version of the option.
--
-- 'dbSecurityGroupMemberships', 'option_dbSecurityGroupMemberships' - If the option requires access to a port, then this DB security group
-- allows access to the port.
--
-- 'optionDescription', 'option_optionDescription' - The description of the option.
--
-- 'port', 'option_port' - If required, the port configured for this option to use.
--
-- 'optionSettings', 'option_optionSettings' - The option settings for this option.
--
-- 'persistent', 'option_persistent' - Indicate if this option is persistent.
--
-- 'vpcSecurityGroupMemberships', 'option_vpcSecurityGroupMemberships' - If the option requires access to a port, then this VPC security group
-- allows access to the port.
--
-- 'permanent', 'option_permanent' - Indicate if this option is permanent.
newOption ::
  Option
newOption =
  Option'
    { optionName = Prelude.Nothing,
      optionVersion = Prelude.Nothing,
      dbSecurityGroupMemberships = Prelude.Nothing,
      optionDescription = Prelude.Nothing,
      port = Prelude.Nothing,
      optionSettings = Prelude.Nothing,
      persistent = Prelude.Nothing,
      vpcSecurityGroupMemberships = Prelude.Nothing,
      permanent = Prelude.Nothing
    }

-- | The name of the option.
option_optionName :: Lens.Lens' Option (Prelude.Maybe Prelude.Text)
option_optionName = Lens.lens (\Option' {optionName} -> optionName) (\s@Option' {} a -> s {optionName = a} :: Option)

-- | The version of the option.
option_optionVersion :: Lens.Lens' Option (Prelude.Maybe Prelude.Text)
option_optionVersion = Lens.lens (\Option' {optionVersion} -> optionVersion) (\s@Option' {} a -> s {optionVersion = a} :: Option)

-- | If the option requires access to a port, then this DB security group
-- allows access to the port.
option_dbSecurityGroupMemberships :: Lens.Lens' Option (Prelude.Maybe [DBSecurityGroupMembership])
option_dbSecurityGroupMemberships = Lens.lens (\Option' {dbSecurityGroupMemberships} -> dbSecurityGroupMemberships) (\s@Option' {} a -> s {dbSecurityGroupMemberships = a} :: Option) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the option.
option_optionDescription :: Lens.Lens' Option (Prelude.Maybe Prelude.Text)
option_optionDescription = Lens.lens (\Option' {optionDescription} -> optionDescription) (\s@Option' {} a -> s {optionDescription = a} :: Option)

-- | If required, the port configured for this option to use.
option_port :: Lens.Lens' Option (Prelude.Maybe Prelude.Int)
option_port = Lens.lens (\Option' {port} -> port) (\s@Option' {} a -> s {port = a} :: Option)

-- | The option settings for this option.
option_optionSettings :: Lens.Lens' Option (Prelude.Maybe [OptionSetting])
option_optionSettings = Lens.lens (\Option' {optionSettings} -> optionSettings) (\s@Option' {} a -> s {optionSettings = a} :: Option) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicate if this option is persistent.
option_persistent :: Lens.Lens' Option (Prelude.Maybe Prelude.Bool)
option_persistent = Lens.lens (\Option' {persistent} -> persistent) (\s@Option' {} a -> s {persistent = a} :: Option)

-- | If the option requires access to a port, then this VPC security group
-- allows access to the port.
option_vpcSecurityGroupMemberships :: Lens.Lens' Option (Prelude.Maybe [VpcSecurityGroupMembership])
option_vpcSecurityGroupMemberships = Lens.lens (\Option' {vpcSecurityGroupMemberships} -> vpcSecurityGroupMemberships) (\s@Option' {} a -> s {vpcSecurityGroupMemberships = a} :: Option) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicate if this option is permanent.
option_permanent :: Lens.Lens' Option (Prelude.Maybe Prelude.Bool)
option_permanent = Lens.lens (\Option' {permanent} -> permanent) (\s@Option' {} a -> s {permanent = a} :: Option)

instance Prelude.FromXML Option where
  parseXML x =
    Option'
      Prelude.<$> (x Prelude..@? "OptionName")
      Prelude.<*> (x Prelude..@? "OptionVersion")
      Prelude.<*> ( x Prelude..@? "DBSecurityGroupMemberships"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "DBSecurityGroup")
                  )
      Prelude.<*> (x Prelude..@? "OptionDescription")
      Prelude.<*> (x Prelude..@? "Port")
      Prelude.<*> ( x Prelude..@? "OptionSettings"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "OptionSetting")
                  )
      Prelude.<*> (x Prelude..@? "Persistent")
      Prelude.<*> ( x Prelude..@? "VpcSecurityGroupMemberships"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may
                        (Prelude.parseXMLList "VpcSecurityGroupMembership")
                  )
      Prelude.<*> (x Prelude..@? "Permanent")

instance Prelude.Hashable Option

instance Prelude.NFData Option
