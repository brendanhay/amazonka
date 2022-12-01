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
-- Module      : Amazonka.AlexaBusiness.GetConferencePreference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the existing conference preferences.
module Amazonka.AlexaBusiness.GetConferencePreference
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConferencePreference' smart constructor.
data GetConferencePreference = GetConferencePreference'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConferencePreferenceResponse'
            Prelude.<$> (x Core..?> "Preference")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConferencePreference where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetConferencePreference where
  rnf _ = ()

instance Core.ToHeaders GetConferencePreference where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.GetConferencePreference" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetConferencePreference where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath GetConferencePreference where
  toPath = Prelude.const "/"

instance Core.ToQuery GetConferencePreference where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConferencePreferenceResponse' smart constructor.
data GetConferencePreferenceResponse = GetConferencePreferenceResponse'
  { -- | The conference preference.
    preference :: Prelude.Maybe ConferencePreference,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetConferencePreferenceResponse
newGetConferencePreferenceResponse pHttpStatus_ =
  GetConferencePreferenceResponse'
    { preference =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The conference preference.
getConferencePreferenceResponse_preference :: Lens.Lens' GetConferencePreferenceResponse (Prelude.Maybe ConferencePreference)
getConferencePreferenceResponse_preference = Lens.lens (\GetConferencePreferenceResponse' {preference} -> preference) (\s@GetConferencePreferenceResponse' {} a -> s {preference = a} :: GetConferencePreferenceResponse)

-- | The response's http status code.
getConferencePreferenceResponse_httpStatus :: Lens.Lens' GetConferencePreferenceResponse Prelude.Int
getConferencePreferenceResponse_httpStatus = Lens.lens (\GetConferencePreferenceResponse' {httpStatus} -> httpStatus) (\s@GetConferencePreferenceResponse' {} a -> s {httpStatus = a} :: GetConferencePreferenceResponse)

instance
  Prelude.NFData
    GetConferencePreferenceResponse
  where
  rnf GetConferencePreferenceResponse' {..} =
    Prelude.rnf preference
      `Prelude.seq` Prelude.rnf httpStatus
