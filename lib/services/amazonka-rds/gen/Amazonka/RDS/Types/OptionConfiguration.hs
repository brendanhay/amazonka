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
-- Module      : Amazonka.RDS.Types.OptionConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.OptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types.OptionSetting

-- | A list of all available options
--
-- /See:/ 'newOptionConfiguration' smart constructor.
data OptionConfiguration = OptionConfiguration'
  { -- | A list of DBSecurityGroupMembership name strings used for this option.
    dbSecurityGroupMemberships :: Prelude.Maybe [Prelude.Text],
    -- | The option settings to include in an option group.
    optionSettings :: Prelude.Maybe [OptionSetting],
    -- | The version for the option.
    optionVersion :: Prelude.Maybe Prelude.Text,
    -- | The optional port for the option.
    port :: Prelude.Maybe Prelude.Int,
    -- | A list of VpcSecurityGroupMembership name strings used for this option.
    vpcSecurityGroupMemberships :: Prelude.Maybe [Prelude.Text],
    -- | The configuration of options to include in a group.
    optionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbSecurityGroupMemberships', 'optionConfiguration_dbSecurityGroupMemberships' - A list of DBSecurityGroupMembership name strings used for this option.
--
-- 'optionSettings', 'optionConfiguration_optionSettings' - The option settings to include in an option group.
--
-- 'optionVersion', 'optionConfiguration_optionVersion' - The version for the option.
--
-- 'port', 'optionConfiguration_port' - The optional port for the option.
--
-- 'vpcSecurityGroupMemberships', 'optionConfiguration_vpcSecurityGroupMemberships' - A list of VpcSecurityGroupMembership name strings used for this option.
--
-- 'optionName', 'optionConfiguration_optionName' - The configuration of options to include in a group.
newOptionConfiguration ::
  -- | 'optionName'
  Prelude.Text ->
  OptionConfiguration
newOptionConfiguration pOptionName_ =
  OptionConfiguration'
    { dbSecurityGroupMemberships =
        Prelude.Nothing,
      optionSettings = Prelude.Nothing,
      optionVersion = Prelude.Nothing,
      port = Prelude.Nothing,
      vpcSecurityGroupMemberships = Prelude.Nothing,
      optionName = pOptionName_
    }

-- | A list of DBSecurityGroupMembership name strings used for this option.
optionConfiguration_dbSecurityGroupMemberships :: Lens.Lens' OptionConfiguration (Prelude.Maybe [Prelude.Text])
optionConfiguration_dbSecurityGroupMemberships = Lens.lens (\OptionConfiguration' {dbSecurityGroupMemberships} -> dbSecurityGroupMemberships) (\s@OptionConfiguration' {} a -> s {dbSecurityGroupMemberships = a} :: OptionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The option settings to include in an option group.
optionConfiguration_optionSettings :: Lens.Lens' OptionConfiguration (Prelude.Maybe [OptionSetting])
optionConfiguration_optionSettings = Lens.lens (\OptionConfiguration' {optionSettings} -> optionSettings) (\s@OptionConfiguration' {} a -> s {optionSettings = a} :: OptionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The version for the option.
optionConfiguration_optionVersion :: Lens.Lens' OptionConfiguration (Prelude.Maybe Prelude.Text)
optionConfiguration_optionVersion = Lens.lens (\OptionConfiguration' {optionVersion} -> optionVersion) (\s@OptionConfiguration' {} a -> s {optionVersion = a} :: OptionConfiguration)

-- | The optional port for the option.
optionConfiguration_port :: Lens.Lens' OptionConfiguration (Prelude.Maybe Prelude.Int)
optionConfiguration_port = Lens.lens (\OptionConfiguration' {port} -> port) (\s@OptionConfiguration' {} a -> s {port = a} :: OptionConfiguration)

-- | A list of VpcSecurityGroupMembership name strings used for this option.
optionConfiguration_vpcSecurityGroupMemberships :: Lens.Lens' OptionConfiguration (Prelude.Maybe [Prelude.Text])
optionConfiguration_vpcSecurityGroupMemberships = Lens.lens (\OptionConfiguration' {vpcSecurityGroupMemberships} -> vpcSecurityGroupMemberships) (\s@OptionConfiguration' {} a -> s {vpcSecurityGroupMemberships = a} :: OptionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The configuration of options to include in a group.
optionConfiguration_optionName :: Lens.Lens' OptionConfiguration Prelude.Text
optionConfiguration_optionName = Lens.lens (\OptionConfiguration' {optionName} -> optionName) (\s@OptionConfiguration' {} a -> s {optionName = a} :: OptionConfiguration)

instance Prelude.Hashable OptionConfiguration where
  hashWithSalt _salt OptionConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` dbSecurityGroupMemberships
      `Prelude.hashWithSalt` optionSettings
      `Prelude.hashWithSalt` optionVersion
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` vpcSecurityGroupMemberships
      `Prelude.hashWithSalt` optionName

instance Prelude.NFData OptionConfiguration where
  rnf OptionConfiguration' {..} =
    Prelude.rnf dbSecurityGroupMemberships `Prelude.seq`
      Prelude.rnf optionSettings `Prelude.seq`
        Prelude.rnf optionVersion `Prelude.seq`
          Prelude.rnf port `Prelude.seq`
            Prelude.rnf vpcSecurityGroupMemberships `Prelude.seq`
              Prelude.rnf optionName

instance Data.ToQuery OptionConfiguration where
  toQuery OptionConfiguration' {..} =
    Prelude.mconcat
      [ "DBSecurityGroupMemberships"
          Data.=: Data.toQuery
            ( Data.toQueryList "DBSecurityGroupName"
                Prelude.<$> dbSecurityGroupMemberships
            ),
        "OptionSettings"
          Data.=: Data.toQuery
            ( Data.toQueryList "OptionSetting"
                Prelude.<$> optionSettings
            ),
        "OptionVersion" Data.=: optionVersion,
        "Port" Data.=: port,
        "VpcSecurityGroupMemberships"
          Data.=: Data.toQuery
            ( Data.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupMemberships
            ),
        "OptionName" Data.=: optionName
      ]
