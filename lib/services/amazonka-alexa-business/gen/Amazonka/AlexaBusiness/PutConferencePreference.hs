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
-- Module      : Amazonka.AlexaBusiness.PutConferencePreference
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the conference preferences on a specific conference provider at the
-- account level.
module Amazonka.AlexaBusiness.PutConferencePreference
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutConferencePreference' smart constructor.
data PutConferencePreference = PutConferencePreference'
  { -- | The conference preference of a specific conference provider.
    conferencePreference :: ConferencePreference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutConferencePreferenceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutConferencePreference where
  hashWithSalt _salt PutConferencePreference' {..} =
    _salt `Prelude.hashWithSalt` conferencePreference

instance Prelude.NFData PutConferencePreference where
  rnf PutConferencePreference' {..} =
    Prelude.rnf conferencePreference

instance Data.ToHeaders PutConferencePreference where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.PutConferencePreference" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutConferencePreference where
  toJSON PutConferencePreference' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ConferencePreference"
                  Data..= conferencePreference
              )
          ]
      )

instance Data.ToPath PutConferencePreference where
  toPath = Prelude.const "/"

instance Data.ToQuery PutConferencePreference where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutConferencePreferenceResponse' smart constructor.
data PutConferencePreferenceResponse = PutConferencePreferenceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutConferencePreferenceResponse
newPutConferencePreferenceResponse pHttpStatus_ =
  PutConferencePreferenceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putConferencePreferenceResponse_httpStatus :: Lens.Lens' PutConferencePreferenceResponse Prelude.Int
putConferencePreferenceResponse_httpStatus = Lens.lens (\PutConferencePreferenceResponse' {httpStatus} -> httpStatus) (\s@PutConferencePreferenceResponse' {} a -> s {httpStatus = a} :: PutConferencePreferenceResponse)

instance
  Prelude.NFData
    PutConferencePreferenceResponse
  where
  rnf PutConferencePreferenceResponse' {..} =
    Prelude.rnf httpStatus
