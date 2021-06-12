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
-- Module      : Network.AWS.AlexaBusiness.PutConferencePreference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the conference preferences on a specific conference provider at the
-- account level.
module Network.AWS.AlexaBusiness.PutConferencePreference
  ( -- * Creating a Request
    PutConferencePreference (..),
    newPutConferencePreference,

    -- * Request Lenses
    putConferencePreference_conferencePreference,

    -- * Destructuring the Response
    PutConferencePreferenceResponse (..),
    newPutConferencePreferenceResponse,

    -- * Response Lenses
    putConferencePreferenceResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutConferencePreference' smart constructor.
data PutConferencePreference = PutConferencePreference'
  { -- | The conference preference of a specific conference provider.
    conferencePreference :: ConferencePreference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutConferencePreference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conferencePreference', 'putConferencePreference_conferencePreference' - The conference preference of a specific conference provider.
newPutConferencePreference ::
  -- | 'conferencePreference'
  ConferencePreference ->
  PutConferencePreference
newPutConferencePreference pConferencePreference_ =
  PutConferencePreference'
    { conferencePreference =
        pConferencePreference_
    }

-- | The conference preference of a specific conference provider.
putConferencePreference_conferencePreference :: Lens.Lens' PutConferencePreference ConferencePreference
putConferencePreference_conferencePreference = Lens.lens (\PutConferencePreference' {conferencePreference} -> conferencePreference) (\s@PutConferencePreference' {} a -> s {conferencePreference = a} :: PutConferencePreference)

instance Core.AWSRequest PutConferencePreference where
  type
    AWSResponse PutConferencePreference =
      PutConferencePreferenceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutConferencePreferenceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutConferencePreference

instance Core.NFData PutConferencePreference

instance Core.ToHeaders PutConferencePreference where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.PutConferencePreference" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutConferencePreference where
  toJSON PutConferencePreference' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConferencePreference"
                  Core..= conferencePreference
              )
          ]
      )

instance Core.ToPath PutConferencePreference where
  toPath = Core.const "/"

instance Core.ToQuery PutConferencePreference where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutConferencePreferenceResponse' smart constructor.
data PutConferencePreferenceResponse = PutConferencePreferenceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutConferencePreferenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putConferencePreferenceResponse_httpStatus' - The response's http status code.
newPutConferencePreferenceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutConferencePreferenceResponse
newPutConferencePreferenceResponse pHttpStatus_ =
  PutConferencePreferenceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putConferencePreferenceResponse_httpStatus :: Lens.Lens' PutConferencePreferenceResponse Core.Int
putConferencePreferenceResponse_httpStatus = Lens.lens (\PutConferencePreferenceResponse' {httpStatus} -> httpStatus) (\s@PutConferencePreferenceResponse' {} a -> s {httpStatus = a} :: PutConferencePreferenceResponse)

instance Core.NFData PutConferencePreferenceResponse
