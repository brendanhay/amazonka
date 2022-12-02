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
-- Module      : Amazonka.ResourceGroups.Types.GroupConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.GroupConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResourceGroups.Types.GroupConfigurationItem
import Amazonka.ResourceGroups.Types.GroupConfigurationStatus

-- | A service configuration associated with a resource group. The
-- configuration options are determined by the AWS service that defines the
-- @Type@, and specifies which resources can be included in the group. You
-- can add a service configuration when you create the group by using
-- CreateGroup, or later by using the PutGroupConfiguration operation. For
-- details about group service configuration syntax, see
-- <https://docs.aws.amazon.com/ARG/latest/APIReference/about-slg.html Service configurations for resource groups>.
--
-- /See:/ 'newGroupConfiguration' smart constructor.
data GroupConfiguration = GroupConfiguration'
  { -- | If present, the new configuration that is in the process of being
    -- applied to the group.
    proposedConfiguration :: Prelude.Maybe [GroupConfigurationItem],
    -- | The configuration currently associated with the group and in effect.
    configuration :: Prelude.Maybe [GroupConfigurationItem],
    -- | The current status of an attempt to update the group configuration.
    status :: Prelude.Maybe GroupConfigurationStatus,
    -- | If present, the reason why a request to update the group configuration
    -- failed.
    failureReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'proposedConfiguration', 'groupConfiguration_proposedConfiguration' - If present, the new configuration that is in the process of being
-- applied to the group.
--
-- 'configuration', 'groupConfiguration_configuration' - The configuration currently associated with the group and in effect.
--
-- 'status', 'groupConfiguration_status' - The current status of an attempt to update the group configuration.
--
-- 'failureReason', 'groupConfiguration_failureReason' - If present, the reason why a request to update the group configuration
-- failed.
newGroupConfiguration ::
  GroupConfiguration
newGroupConfiguration =
  GroupConfiguration'
    { proposedConfiguration =
        Prelude.Nothing,
      configuration = Prelude.Nothing,
      status = Prelude.Nothing,
      failureReason = Prelude.Nothing
    }

-- | If present, the new configuration that is in the process of being
-- applied to the group.
groupConfiguration_proposedConfiguration :: Lens.Lens' GroupConfiguration (Prelude.Maybe [GroupConfigurationItem])
groupConfiguration_proposedConfiguration = Lens.lens (\GroupConfiguration' {proposedConfiguration} -> proposedConfiguration) (\s@GroupConfiguration' {} a -> s {proposedConfiguration = a} :: GroupConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The configuration currently associated with the group and in effect.
groupConfiguration_configuration :: Lens.Lens' GroupConfiguration (Prelude.Maybe [GroupConfigurationItem])
groupConfiguration_configuration = Lens.lens (\GroupConfiguration' {configuration} -> configuration) (\s@GroupConfiguration' {} a -> s {configuration = a} :: GroupConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The current status of an attempt to update the group configuration.
groupConfiguration_status :: Lens.Lens' GroupConfiguration (Prelude.Maybe GroupConfigurationStatus)
groupConfiguration_status = Lens.lens (\GroupConfiguration' {status} -> status) (\s@GroupConfiguration' {} a -> s {status = a} :: GroupConfiguration)

-- | If present, the reason why a request to update the group configuration
-- failed.
groupConfiguration_failureReason :: Lens.Lens' GroupConfiguration (Prelude.Maybe Prelude.Text)
groupConfiguration_failureReason = Lens.lens (\GroupConfiguration' {failureReason} -> failureReason) (\s@GroupConfiguration' {} a -> s {failureReason = a} :: GroupConfiguration)

instance Data.FromJSON GroupConfiguration where
  parseJSON =
    Data.withObject
      "GroupConfiguration"
      ( \x ->
          GroupConfiguration'
            Prelude.<$> ( x Data..:? "ProposedConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Configuration" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "FailureReason")
      )

instance Prelude.Hashable GroupConfiguration where
  hashWithSalt _salt GroupConfiguration' {..} =
    _salt `Prelude.hashWithSalt` proposedConfiguration
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData GroupConfiguration where
  rnf GroupConfiguration' {..} =
    Prelude.rnf proposedConfiguration
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf failureReason
