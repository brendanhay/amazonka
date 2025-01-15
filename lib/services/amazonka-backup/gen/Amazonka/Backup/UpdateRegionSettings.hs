{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Backup.UpdateRegionSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the current service opt-in settings for the Region. If
-- service-opt-in is enabled for a service, Backup tries to protect that
-- service\'s resources in this Region, when the resource is included in an
-- on-demand backup or scheduled backup plan. Otherwise, Backup does not
-- try to protect that service\'s resources in this Region. Use the
-- @DescribeRegionSettings@ API to determine the resource types that are
-- supported.
module Amazonka.Backup.UpdateRegionSettings
  ( -- * Creating a Request
    UpdateRegionSettings (..),
    newUpdateRegionSettings,

    -- * Request Lenses
    updateRegionSettings_resourceTypeManagementPreference,
    updateRegionSettings_resourceTypeOptInPreference,

    -- * Destructuring the Response
    UpdateRegionSettingsResponse (..),
    newUpdateRegionSettingsResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRegionSettings' smart constructor.
data UpdateRegionSettings = UpdateRegionSettings'
  { -- | Enables or disables full Backup management of backups for a resource
    -- type. To enable full Backup management for DynamoDB along with
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/advanced-ddb-backup.html Backup\'s advanced DynamoDB backup features>,
    -- follow the procedure to
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/advanced-ddb-backup.html#advanced-ddb-backup-enable-cli enable advanced DynamoDB backup programmatically>.
    resourceTypeManagementPreference :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool),
    -- | Updates the list of services along with the opt-in preferences for the
    -- Region.
    resourceTypeOptInPreference :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRegionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTypeManagementPreference', 'updateRegionSettings_resourceTypeManagementPreference' - Enables or disables full Backup management of backups for a resource
-- type. To enable full Backup management for DynamoDB along with
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/advanced-ddb-backup.html Backup\'s advanced DynamoDB backup features>,
-- follow the procedure to
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/advanced-ddb-backup.html#advanced-ddb-backup-enable-cli enable advanced DynamoDB backup programmatically>.
--
-- 'resourceTypeOptInPreference', 'updateRegionSettings_resourceTypeOptInPreference' - Updates the list of services along with the opt-in preferences for the
-- Region.
newUpdateRegionSettings ::
  UpdateRegionSettings
newUpdateRegionSettings =
  UpdateRegionSettings'
    { resourceTypeManagementPreference =
        Prelude.Nothing,
      resourceTypeOptInPreference = Prelude.Nothing
    }

-- | Enables or disables full Backup management of backups for a resource
-- type. To enable full Backup management for DynamoDB along with
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/advanced-ddb-backup.html Backup\'s advanced DynamoDB backup features>,
-- follow the procedure to
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/advanced-ddb-backup.html#advanced-ddb-backup-enable-cli enable advanced DynamoDB backup programmatically>.
updateRegionSettings_resourceTypeManagementPreference :: Lens.Lens' UpdateRegionSettings (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool))
updateRegionSettings_resourceTypeManagementPreference = Lens.lens (\UpdateRegionSettings' {resourceTypeManagementPreference} -> resourceTypeManagementPreference) (\s@UpdateRegionSettings' {} a -> s {resourceTypeManagementPreference = a} :: UpdateRegionSettings) Prelude.. Lens.mapping Lens.coerced

-- | Updates the list of services along with the opt-in preferences for the
-- Region.
updateRegionSettings_resourceTypeOptInPreference :: Lens.Lens' UpdateRegionSettings (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool))
updateRegionSettings_resourceTypeOptInPreference = Lens.lens (\UpdateRegionSettings' {resourceTypeOptInPreference} -> resourceTypeOptInPreference) (\s@UpdateRegionSettings' {} a -> s {resourceTypeOptInPreference = a} :: UpdateRegionSettings) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest UpdateRegionSettings where
  type
    AWSResponse UpdateRegionSettings =
      UpdateRegionSettingsResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateRegionSettingsResponse'

instance Prelude.Hashable UpdateRegionSettings where
  hashWithSalt _salt UpdateRegionSettings' {..} =
    _salt
      `Prelude.hashWithSalt` resourceTypeManagementPreference
      `Prelude.hashWithSalt` resourceTypeOptInPreference

instance Prelude.NFData UpdateRegionSettings where
  rnf UpdateRegionSettings' {..} =
    Prelude.rnf resourceTypeManagementPreference `Prelude.seq`
      Prelude.rnf resourceTypeOptInPreference

instance Data.ToHeaders UpdateRegionSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRegionSettings where
  toJSON UpdateRegionSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceTypeManagementPreference" Data..=)
              Prelude.<$> resourceTypeManagementPreference,
            ("ResourceTypeOptInPreference" Data..=)
              Prelude.<$> resourceTypeOptInPreference
          ]
      )

instance Data.ToPath UpdateRegionSettings where
  toPath = Prelude.const "/account-settings"

instance Data.ToQuery UpdateRegionSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRegionSettingsResponse' smart constructor.
data UpdateRegionSettingsResponse = UpdateRegionSettingsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRegionSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateRegionSettingsResponse ::
  UpdateRegionSettingsResponse
newUpdateRegionSettingsResponse =
  UpdateRegionSettingsResponse'

instance Prelude.NFData UpdateRegionSettingsResponse where
  rnf _ = ()
