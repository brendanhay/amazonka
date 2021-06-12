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
-- Module      : Network.AWS.RDS.Types.OptionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionConfiguration where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.RDS.Types.OptionSetting

-- | A list of all available options
--
-- /See:/ 'newOptionConfiguration' smart constructor.
data OptionConfiguration = OptionConfiguration'
  { -- | The version for the option.
    optionVersion :: Core.Maybe Core.Text,
    -- | A list of DBSecurityGroupMembership name strings used for this option.
    dbSecurityGroupMemberships :: Core.Maybe [Core.Text],
    -- | The optional port for the option.
    port :: Core.Maybe Core.Int,
    -- | The option settings to include in an option group.
    optionSettings :: Core.Maybe [OptionSetting],
    -- | A list of VpcSecurityGroupMembership name strings used for this option.
    vpcSecurityGroupMemberships :: Core.Maybe [Core.Text],
    -- | The configuration of options to include in a group.
    optionName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionVersion', 'optionConfiguration_optionVersion' - The version for the option.
--
-- 'dbSecurityGroupMemberships', 'optionConfiguration_dbSecurityGroupMemberships' - A list of DBSecurityGroupMembership name strings used for this option.
--
-- 'port', 'optionConfiguration_port' - The optional port for the option.
--
-- 'optionSettings', 'optionConfiguration_optionSettings' - The option settings to include in an option group.
--
-- 'vpcSecurityGroupMemberships', 'optionConfiguration_vpcSecurityGroupMemberships' - A list of VpcSecurityGroupMembership name strings used for this option.
--
-- 'optionName', 'optionConfiguration_optionName' - The configuration of options to include in a group.
newOptionConfiguration ::
  -- | 'optionName'
  Core.Text ->
  OptionConfiguration
newOptionConfiguration pOptionName_ =
  OptionConfiguration'
    { optionVersion = Core.Nothing,
      dbSecurityGroupMemberships = Core.Nothing,
      port = Core.Nothing,
      optionSettings = Core.Nothing,
      vpcSecurityGroupMemberships = Core.Nothing,
      optionName = pOptionName_
    }

-- | The version for the option.
optionConfiguration_optionVersion :: Lens.Lens' OptionConfiguration (Core.Maybe Core.Text)
optionConfiguration_optionVersion = Lens.lens (\OptionConfiguration' {optionVersion} -> optionVersion) (\s@OptionConfiguration' {} a -> s {optionVersion = a} :: OptionConfiguration)

-- | A list of DBSecurityGroupMembership name strings used for this option.
optionConfiguration_dbSecurityGroupMemberships :: Lens.Lens' OptionConfiguration (Core.Maybe [Core.Text])
optionConfiguration_dbSecurityGroupMemberships = Lens.lens (\OptionConfiguration' {dbSecurityGroupMemberships} -> dbSecurityGroupMemberships) (\s@OptionConfiguration' {} a -> s {dbSecurityGroupMemberships = a} :: OptionConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The optional port for the option.
optionConfiguration_port :: Lens.Lens' OptionConfiguration (Core.Maybe Core.Int)
optionConfiguration_port = Lens.lens (\OptionConfiguration' {port} -> port) (\s@OptionConfiguration' {} a -> s {port = a} :: OptionConfiguration)

-- | The option settings to include in an option group.
optionConfiguration_optionSettings :: Lens.Lens' OptionConfiguration (Core.Maybe [OptionSetting])
optionConfiguration_optionSettings = Lens.lens (\OptionConfiguration' {optionSettings} -> optionSettings) (\s@OptionConfiguration' {} a -> s {optionSettings = a} :: OptionConfiguration) Core.. Lens.mapping Lens._Coerce

-- | A list of VpcSecurityGroupMembership name strings used for this option.
optionConfiguration_vpcSecurityGroupMemberships :: Lens.Lens' OptionConfiguration (Core.Maybe [Core.Text])
optionConfiguration_vpcSecurityGroupMemberships = Lens.lens (\OptionConfiguration' {vpcSecurityGroupMemberships} -> vpcSecurityGroupMemberships) (\s@OptionConfiguration' {} a -> s {vpcSecurityGroupMemberships = a} :: OptionConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The configuration of options to include in a group.
optionConfiguration_optionName :: Lens.Lens' OptionConfiguration Core.Text
optionConfiguration_optionName = Lens.lens (\OptionConfiguration' {optionName} -> optionName) (\s@OptionConfiguration' {} a -> s {optionName = a} :: OptionConfiguration)

instance Core.Hashable OptionConfiguration

instance Core.NFData OptionConfiguration

instance Core.ToQuery OptionConfiguration where
  toQuery OptionConfiguration' {..} =
    Core.mconcat
      [ "OptionVersion" Core.=: optionVersion,
        "DBSecurityGroupMemberships"
          Core.=: Core.toQuery
            ( Core.toQueryList "DBSecurityGroupName"
                Core.<$> dbSecurityGroupMemberships
            ),
        "Port" Core.=: port,
        "OptionSettings"
          Core.=: Core.toQuery
            ( Core.toQueryList "OptionSetting"
                Core.<$> optionSettings
            ),
        "VpcSecurityGroupMemberships"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Core.<$> vpcSecurityGroupMemberships
            ),
        "OptionName" Core.=: optionName
      ]
