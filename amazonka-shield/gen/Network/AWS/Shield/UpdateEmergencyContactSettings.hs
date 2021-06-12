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
-- Module      : Network.AWS.Shield.UpdateEmergencyContactSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of the list of email addresses and phone numbers
-- that the DDoS Response Team (DRT) can use to contact you if you have
-- proactive engagement enabled, for escalations to the DRT and to initiate
-- proactive customer support.
module Network.AWS.Shield.UpdateEmergencyContactSettings
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Shield.Types

-- | /See:/ 'newUpdateEmergencyContactSettings' smart constructor.
data UpdateEmergencyContactSettings = UpdateEmergencyContactSettings'
  { -- | A list of email addresses and phone numbers that the DDoS Response Team
    -- (DRT) can use to contact you if you have proactive engagement enabled,
    -- for escalations to the DRT and to initiate proactive customer support.
    --
    -- If you have proactive engagement enabled, the contact list must include
    -- at least one phone number.
    emergencyContactList :: Core.Maybe [EmergencyContact]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateEmergencyContactSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emergencyContactList', 'updateEmergencyContactSettings_emergencyContactList' - A list of email addresses and phone numbers that the DDoS Response Team
-- (DRT) can use to contact you if you have proactive engagement enabled,
-- for escalations to the DRT and to initiate proactive customer support.
--
-- If you have proactive engagement enabled, the contact list must include
-- at least one phone number.
newUpdateEmergencyContactSettings ::
  UpdateEmergencyContactSettings
newUpdateEmergencyContactSettings =
  UpdateEmergencyContactSettings'
    { emergencyContactList =
        Core.Nothing
    }

-- | A list of email addresses and phone numbers that the DDoS Response Team
-- (DRT) can use to contact you if you have proactive engagement enabled,
-- for escalations to the DRT and to initiate proactive customer support.
--
-- If you have proactive engagement enabled, the contact list must include
-- at least one phone number.
updateEmergencyContactSettings_emergencyContactList :: Lens.Lens' UpdateEmergencyContactSettings (Core.Maybe [EmergencyContact])
updateEmergencyContactSettings_emergencyContactList = Lens.lens (\UpdateEmergencyContactSettings' {emergencyContactList} -> emergencyContactList) (\s@UpdateEmergencyContactSettings' {} a -> s {emergencyContactList = a} :: UpdateEmergencyContactSettings) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSRequest
    UpdateEmergencyContactSettings
  where
  type
    AWSResponse UpdateEmergencyContactSettings =
      UpdateEmergencyContactSettingsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEmergencyContactSettingsResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateEmergencyContactSettings

instance Core.NFData UpdateEmergencyContactSettings

instance
  Core.ToHeaders
    UpdateEmergencyContactSettings
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShield_20160616.UpdateEmergencyContactSettings" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateEmergencyContactSettings where
  toJSON UpdateEmergencyContactSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EmergencyContactList" Core..=)
              Core.<$> emergencyContactList
          ]
      )

instance Core.ToPath UpdateEmergencyContactSettings where
  toPath = Core.const "/"

instance Core.ToQuery UpdateEmergencyContactSettings where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateEmergencyContactSettingsResponse' smart constructor.
data UpdateEmergencyContactSettingsResponse = UpdateEmergencyContactSettingsResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateEmergencyContactSettingsResponse
newUpdateEmergencyContactSettingsResponse
  pHttpStatus_ =
    UpdateEmergencyContactSettingsResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateEmergencyContactSettingsResponse_httpStatus :: Lens.Lens' UpdateEmergencyContactSettingsResponse Core.Int
updateEmergencyContactSettingsResponse_httpStatus = Lens.lens (\UpdateEmergencyContactSettingsResponse' {httpStatus} -> httpStatus) (\s@UpdateEmergencyContactSettingsResponse' {} a -> s {httpStatus = a} :: UpdateEmergencyContactSettingsResponse)

instance
  Core.NFData
    UpdateEmergencyContactSettingsResponse
