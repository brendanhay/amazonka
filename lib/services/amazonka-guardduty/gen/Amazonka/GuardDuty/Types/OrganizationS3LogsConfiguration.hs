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
-- Module      : Amazonka.GuardDuty.Types.OrganizationS3LogsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.OrganizationS3LogsConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes whether S3 data event logs will be automatically enabled for
-- new members of the organization.
--
-- /See:/ 'newOrganizationS3LogsConfiguration' smart constructor.
data OrganizationS3LogsConfiguration = OrganizationS3LogsConfiguration'
  { -- | A value that contains information on whether S3 data event logs will be
    -- enabled automatically as a data source for the organization.
    autoEnable :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationS3LogsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoEnable', 'organizationS3LogsConfiguration_autoEnable' - A value that contains information on whether S3 data event logs will be
-- enabled automatically as a data source for the organization.
newOrganizationS3LogsConfiguration ::
  -- | 'autoEnable'
  Prelude.Bool ->
  OrganizationS3LogsConfiguration
newOrganizationS3LogsConfiguration pAutoEnable_ =
  OrganizationS3LogsConfiguration'
    { autoEnable =
        pAutoEnable_
    }

-- | A value that contains information on whether S3 data event logs will be
-- enabled automatically as a data source for the organization.
organizationS3LogsConfiguration_autoEnable :: Lens.Lens' OrganizationS3LogsConfiguration Prelude.Bool
organizationS3LogsConfiguration_autoEnable = Lens.lens (\OrganizationS3LogsConfiguration' {autoEnable} -> autoEnable) (\s@OrganizationS3LogsConfiguration' {} a -> s {autoEnable = a} :: OrganizationS3LogsConfiguration)

instance
  Prelude.Hashable
    OrganizationS3LogsConfiguration
  where
  hashWithSalt
    _salt
    OrganizationS3LogsConfiguration' {..} =
      _salt `Prelude.hashWithSalt` autoEnable

instance
  Prelude.NFData
    OrganizationS3LogsConfiguration
  where
  rnf OrganizationS3LogsConfiguration' {..} =
    Prelude.rnf autoEnable

instance Data.ToJSON OrganizationS3LogsConfiguration where
  toJSON OrganizationS3LogsConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("autoEnable" Data..= autoEnable)]
      )
