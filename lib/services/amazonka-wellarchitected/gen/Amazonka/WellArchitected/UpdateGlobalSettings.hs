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
-- Module      : Amazonka.WellArchitected.UpdateGlobalSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates whether the Amazon Web Services account is opted into
-- organization sharing features.
module Amazonka.WellArchitected.UpdateGlobalSettings
  ( -- * Creating a Request
    UpdateGlobalSettings (..),
    newUpdateGlobalSettings,

    -- * Request Lenses
    updateGlobalSettings_organizationSharingStatus,

    -- * Destructuring the Response
    UpdateGlobalSettingsResponse (..),
    newUpdateGlobalSettingsResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | /See:/ 'newUpdateGlobalSettings' smart constructor.
data UpdateGlobalSettings = UpdateGlobalSettings'
  { -- | The status of organization sharing settings.
    organizationSharingStatus :: Prelude.Maybe OrganizationSharingStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationSharingStatus', 'updateGlobalSettings_organizationSharingStatus' - The status of organization sharing settings.
newUpdateGlobalSettings ::
  UpdateGlobalSettings
newUpdateGlobalSettings =
  UpdateGlobalSettings'
    { organizationSharingStatus =
        Prelude.Nothing
    }

-- | The status of organization sharing settings.
updateGlobalSettings_organizationSharingStatus :: Lens.Lens' UpdateGlobalSettings (Prelude.Maybe OrganizationSharingStatus)
updateGlobalSettings_organizationSharingStatus = Lens.lens (\UpdateGlobalSettings' {organizationSharingStatus} -> organizationSharingStatus) (\s@UpdateGlobalSettings' {} a -> s {organizationSharingStatus = a} :: UpdateGlobalSettings)

instance Core.AWSRequest UpdateGlobalSettings where
  type
    AWSResponse UpdateGlobalSettings =
      UpdateGlobalSettingsResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateGlobalSettingsResponse'

instance Prelude.Hashable UpdateGlobalSettings where
  hashWithSalt _salt UpdateGlobalSettings' {..} =
    _salt
      `Prelude.hashWithSalt` organizationSharingStatus

instance Prelude.NFData UpdateGlobalSettings where
  rnf UpdateGlobalSettings' {..} =
    Prelude.rnf organizationSharingStatus

instance Data.ToHeaders UpdateGlobalSettings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateGlobalSettings where
  toJSON UpdateGlobalSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OrganizationSharingStatus" Data..=)
              Prelude.<$> organizationSharingStatus
          ]
      )

instance Data.ToPath UpdateGlobalSettings where
  toPath = Prelude.const "/global-settings"

instance Data.ToQuery UpdateGlobalSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGlobalSettingsResponse' smart constructor.
data UpdateGlobalSettingsResponse = UpdateGlobalSettingsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGlobalSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateGlobalSettingsResponse ::
  UpdateGlobalSettingsResponse
newUpdateGlobalSettingsResponse =
  UpdateGlobalSettingsResponse'

instance Prelude.NFData UpdateGlobalSettingsResponse where
  rnf _ = ()
