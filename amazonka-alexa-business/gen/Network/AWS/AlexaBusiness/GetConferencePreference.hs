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
-- Module      : Network.AWS.AlexaBusiness.GetConferencePreference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the existing conference preferences.
module Network.AWS.AlexaBusiness.GetConferencePreference
  ( -- * Creating a Request
    GetConferencePreference (..),
    newGetConferencePreference,

    -- * Destructuring the Response
    GetConferencePreferenceResponse (..),
    newGetConferencePreferenceResponse,

    -- * Response Lenses
    getConferencePreferenceResponse_preference,
    getConferencePreferenceResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConferencePreference' smart constructor.
data GetConferencePreference = GetConferencePreference'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConferencePreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetConferencePreference ::
  GetConferencePreference
newGetConferencePreference = GetConferencePreference'

instance Core.AWSRequest GetConferencePreference where
  type
    AWSResponse GetConferencePreference =
      GetConferencePreferenceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConferencePreferenceResponse'
            Core.<$> (x Core..?> "Preference")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetConferencePreference

instance Core.NFData GetConferencePreference

instance Core.ToHeaders GetConferencePreference where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.GetConferencePreference" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetConferencePreference where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetConferencePreference where
  toPath = Core.const "/"

instance Core.ToQuery GetConferencePreference where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetConferencePreferenceResponse' smart constructor.
data GetConferencePreferenceResponse = GetConferencePreferenceResponse'
  { -- | The conference preference.
    preference :: Core.Maybe ConferencePreference,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConferencePreferenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preference', 'getConferencePreferenceResponse_preference' - The conference preference.
--
-- 'httpStatus', 'getConferencePreferenceResponse_httpStatus' - The response's http status code.
newGetConferencePreferenceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConferencePreferenceResponse
newGetConferencePreferenceResponse pHttpStatus_ =
  GetConferencePreferenceResponse'
    { preference =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The conference preference.
getConferencePreferenceResponse_preference :: Lens.Lens' GetConferencePreferenceResponse (Core.Maybe ConferencePreference)
getConferencePreferenceResponse_preference = Lens.lens (\GetConferencePreferenceResponse' {preference} -> preference) (\s@GetConferencePreferenceResponse' {} a -> s {preference = a} :: GetConferencePreferenceResponse)

-- | The response's http status code.
getConferencePreferenceResponse_httpStatus :: Lens.Lens' GetConferencePreferenceResponse Core.Int
getConferencePreferenceResponse_httpStatus = Lens.lens (\GetConferencePreferenceResponse' {httpStatus} -> httpStatus) (\s@GetConferencePreferenceResponse' {} a -> s {httpStatus = a} :: GetConferencePreferenceResponse)

instance Core.NFData GetConferencePreferenceResponse
