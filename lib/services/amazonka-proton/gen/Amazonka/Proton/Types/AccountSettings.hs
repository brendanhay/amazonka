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
-- Module      : Amazonka.Proton.Types.AccountSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.AccountSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.RepositoryBranch

-- | Proton settings that are used for multiple services in the Amazon Web
-- Services account.
--
-- /See:/ 'newAccountSettings' smart constructor.
data AccountSettings = AccountSettings'
  { -- | The linked repository for pipeline provisioning. Required if you have
    -- environments configured for self-managed provisioning with services that
    -- include pipelines. A linked repository is a repository that has been
    -- registered with Proton. For more information, see CreateRepository.
    pipelineProvisioningRepository :: Prelude.Maybe RepositoryBranch,
    -- | The Amazon Resource Name (ARN) of the service role you want to use for
    -- provisioning pipelines. Assumed by Proton for Amazon Web
    -- Services-managed provisioning, and by customer-owned automation for
    -- self-managed provisioning.
    pipelineServiceRoleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pipelineProvisioningRepository', 'accountSettings_pipelineProvisioningRepository' - The linked repository for pipeline provisioning. Required if you have
-- environments configured for self-managed provisioning with services that
-- include pipelines. A linked repository is a repository that has been
-- registered with Proton. For more information, see CreateRepository.
--
-- 'pipelineServiceRoleArn', 'accountSettings_pipelineServiceRoleArn' - The Amazon Resource Name (ARN) of the service role you want to use for
-- provisioning pipelines. Assumed by Proton for Amazon Web
-- Services-managed provisioning, and by customer-owned automation for
-- self-managed provisioning.
newAccountSettings ::
  AccountSettings
newAccountSettings =
  AccountSettings'
    { pipelineProvisioningRepository =
        Prelude.Nothing,
      pipelineServiceRoleArn = Prelude.Nothing
    }

-- | The linked repository for pipeline provisioning. Required if you have
-- environments configured for self-managed provisioning with services that
-- include pipelines. A linked repository is a repository that has been
-- registered with Proton. For more information, see CreateRepository.
accountSettings_pipelineProvisioningRepository :: Lens.Lens' AccountSettings (Prelude.Maybe RepositoryBranch)
accountSettings_pipelineProvisioningRepository = Lens.lens (\AccountSettings' {pipelineProvisioningRepository} -> pipelineProvisioningRepository) (\s@AccountSettings' {} a -> s {pipelineProvisioningRepository = a} :: AccountSettings)

-- | The Amazon Resource Name (ARN) of the service role you want to use for
-- provisioning pipelines. Assumed by Proton for Amazon Web
-- Services-managed provisioning, and by customer-owned automation for
-- self-managed provisioning.
accountSettings_pipelineServiceRoleArn :: Lens.Lens' AccountSettings (Prelude.Maybe Prelude.Text)
accountSettings_pipelineServiceRoleArn = Lens.lens (\AccountSettings' {pipelineServiceRoleArn} -> pipelineServiceRoleArn) (\s@AccountSettings' {} a -> s {pipelineServiceRoleArn = a} :: AccountSettings)

instance Core.FromJSON AccountSettings where
  parseJSON =
    Core.withObject
      "AccountSettings"
      ( \x ->
          AccountSettings'
            Prelude.<$> (x Core..:? "pipelineProvisioningRepository")
            Prelude.<*> (x Core..:? "pipelineServiceRoleArn")
      )

instance Prelude.Hashable AccountSettings where
  hashWithSalt _salt AccountSettings' {..} =
    _salt
      `Prelude.hashWithSalt` pipelineProvisioningRepository
      `Prelude.hashWithSalt` pipelineServiceRoleArn

instance Prelude.NFData AccountSettings where
  rnf AccountSettings' {..} =
    Prelude.rnf pipelineProvisioningRepository
      `Prelude.seq` Prelude.rnf pipelineServiceRoleArn
