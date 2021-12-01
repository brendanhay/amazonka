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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
    updateRegionSettings_resourceTypeOptInPreference,

    -- * Destructuring the Response
    UpdateRegionSettingsResponse (..),
    newUpdateRegionSettingsResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRegionSettings' smart constructor.
data UpdateRegionSettings = UpdateRegionSettings'
  { -- | Updates the list of services along with the opt-in preferences for the
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
-- 'resourceTypeOptInPreference', 'updateRegionSettings_resourceTypeOptInPreference' - Updates the list of services along with the opt-in preferences for the
-- Region.
newUpdateRegionSettings ::
  UpdateRegionSettings
newUpdateRegionSettings =
  UpdateRegionSettings'
    { resourceTypeOptInPreference =
        Prelude.Nothing
    }

-- | Updates the list of services along with the opt-in preferences for the
-- Region.
updateRegionSettings_resourceTypeOptInPreference :: Lens.Lens' UpdateRegionSettings (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool))
updateRegionSettings_resourceTypeOptInPreference = Lens.lens (\UpdateRegionSettings' {resourceTypeOptInPreference} -> resourceTypeOptInPreference) (\s@UpdateRegionSettings' {} a -> s {resourceTypeOptInPreference = a} :: UpdateRegionSettings) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest UpdateRegionSettings where
  type
    AWSResponse UpdateRegionSettings =
      UpdateRegionSettingsResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveNull UpdateRegionSettingsResponse'

instance Prelude.Hashable UpdateRegionSettings where
  hashWithSalt salt' UpdateRegionSettings' {..} =
    salt'
      `Prelude.hashWithSalt` resourceTypeOptInPreference

instance Prelude.NFData UpdateRegionSettings where
  rnf UpdateRegionSettings' {..} =
    Prelude.rnf resourceTypeOptInPreference

instance Core.ToHeaders UpdateRegionSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRegionSettings where
  toJSON UpdateRegionSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceTypeOptInPreference" Core..=)
              Prelude.<$> resourceTypeOptInPreference
          ]
      )

instance Core.ToPath UpdateRegionSettings where
  toPath = Prelude.const "/account-settings"

instance Core.ToQuery UpdateRegionSettings where
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
