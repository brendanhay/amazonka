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
-- Module      : Amazonka.Shield.UpdateEmergencyContactSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of the list of email addresses and phone numbers
-- that the Shield Response Team (SRT) can use to contact you if you have
-- proactive engagement enabled, for escalations to the SRT and to initiate
-- proactive customer support.
module Amazonka.Shield.UpdateEmergencyContactSettings
  ( -- * Creating a Request
    UpdateEmergencyContactSettings (..),
    newUpdateEmergencyContactSettings,

    -- * Request Lenses
    updateEmergencyContactSettings_emergencyContactList,

    -- * Destructuring the Response
    UpdateEmergencyContactSettingsResponse (..),
    newUpdateEmergencyContactSettingsResponse,

    -- * Response Lenses
    updateEmergencyContactSettingsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Shield.Types

-- | /See:/ 'newUpdateEmergencyContactSettings' smart constructor.
data UpdateEmergencyContactSettings = UpdateEmergencyContactSettings'
  { -- | A list of email addresses and phone numbers that the Shield Response
    -- Team (SRT) can use to contact you if you have proactive engagement
    -- enabled, for escalations to the SRT and to initiate proactive customer
    -- support.
    --
    -- If you have proactive engagement enabled, the contact list must include
    -- at least one phone number.
    emergencyContactList :: Prelude.Maybe [EmergencyContact]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEmergencyContactSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emergencyContactList', 'updateEmergencyContactSettings_emergencyContactList' - A list of email addresses and phone numbers that the Shield Response
-- Team (SRT) can use to contact you if you have proactive engagement
-- enabled, for escalations to the SRT and to initiate proactive customer
-- support.
--
-- If you have proactive engagement enabled, the contact list must include
-- at least one phone number.
newUpdateEmergencyContactSettings ::
  UpdateEmergencyContactSettings
newUpdateEmergencyContactSettings =
  UpdateEmergencyContactSettings'
    { emergencyContactList =
        Prelude.Nothing
    }

-- | A list of email addresses and phone numbers that the Shield Response
-- Team (SRT) can use to contact you if you have proactive engagement
-- enabled, for escalations to the SRT and to initiate proactive customer
-- support.
--
-- If you have proactive engagement enabled, the contact list must include
-- at least one phone number.
updateEmergencyContactSettings_emergencyContactList :: Lens.Lens' UpdateEmergencyContactSettings (Prelude.Maybe [EmergencyContact])
updateEmergencyContactSettings_emergencyContactList = Lens.lens (\UpdateEmergencyContactSettings' {emergencyContactList} -> emergencyContactList) (\s@UpdateEmergencyContactSettings' {} a -> s {emergencyContactList = a} :: UpdateEmergencyContactSettings) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    UpdateEmergencyContactSettings
  where
  type
    AWSResponse UpdateEmergencyContactSettings =
      UpdateEmergencyContactSettingsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEmergencyContactSettingsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateEmergencyContactSettings
  where
  hashWithSalt
    _salt
    UpdateEmergencyContactSettings' {..} =
      _salt `Prelude.hashWithSalt` emergencyContactList

instance
  Prelude.NFData
    UpdateEmergencyContactSettings
  where
  rnf UpdateEmergencyContactSettings' {..} =
    Prelude.rnf emergencyContactList

instance
  Data.ToHeaders
    UpdateEmergencyContactSettings
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShield_20160616.UpdateEmergencyContactSettings" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEmergencyContactSettings where
  toJSON UpdateEmergencyContactSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EmergencyContactList" Data..=)
              Prelude.<$> emergencyContactList
          ]
      )

instance Data.ToPath UpdateEmergencyContactSettings where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEmergencyContactSettings where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEmergencyContactSettingsResponse' smart constructor.
data UpdateEmergencyContactSettingsResponse = UpdateEmergencyContactSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEmergencyContactSettingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEmergencyContactSettingsResponse_httpStatus' - The response's http status code.
newUpdateEmergencyContactSettingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEmergencyContactSettingsResponse
newUpdateEmergencyContactSettingsResponse
  pHttpStatus_ =
    UpdateEmergencyContactSettingsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateEmergencyContactSettingsResponse_httpStatus :: Lens.Lens' UpdateEmergencyContactSettingsResponse Prelude.Int
updateEmergencyContactSettingsResponse_httpStatus = Lens.lens (\UpdateEmergencyContactSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateEmergencyContactSettingsResponse' {} a -> s {httpStatus = a} :: UpdateEmergencyContactSettingsResponse)

instance
  Prelude.NFData
    UpdateEmergencyContactSettingsResponse
  where
  rnf UpdateEmergencyContactSettingsResponse' {..} =
    Prelude.rnf httpStatus
