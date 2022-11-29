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
-- Module      : Amazonka.MechanicalTurk.GetHIT
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetHIT@ operation retrieves the details of the specified HIT.
module Amazonka.MechanicalTurk.GetHIT
  ( -- * Creating a Request
    GetHIT (..),
    newGetHIT,

    -- * Request Lenses
    getHIT_hITId,

    -- * Destructuring the Response
    GetHITResponse (..),
    newGetHITResponse,

    -- * Response Lenses
    getHITResponse_hit,
    getHITResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetHIT' smart constructor.
data GetHIT = GetHIT'
  { -- | The ID of the HIT to be retrieved.
    hITId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHIT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hITId', 'getHIT_hITId' - The ID of the HIT to be retrieved.
newGetHIT ::
  -- | 'hITId'
  Prelude.Text ->
  GetHIT
newGetHIT pHITId_ = GetHIT' {hITId = pHITId_}

-- | The ID of the HIT to be retrieved.
getHIT_hITId :: Lens.Lens' GetHIT Prelude.Text
getHIT_hITId = Lens.lens (\GetHIT' {hITId} -> hITId) (\s@GetHIT' {} a -> s {hITId = a} :: GetHIT)

instance Core.AWSRequest GetHIT where
  type AWSResponse GetHIT = GetHITResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHITResponse'
            Prelude.<$> (x Core..?> "HIT")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetHIT where
  hashWithSalt _salt GetHIT' {..} =
    _salt `Prelude.hashWithSalt` hITId

instance Prelude.NFData GetHIT where
  rnf GetHIT' {..} = Prelude.rnf hITId

instance Core.ToHeaders GetHIT where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.GetHIT" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetHIT where
  toJSON GetHIT' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("HITId" Core..= hITId)]
      )

instance Core.ToPath GetHIT where
  toPath = Prelude.const "/"

instance Core.ToQuery GetHIT where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetHITResponse' smart constructor.
data GetHITResponse = GetHITResponse'
  { -- | Contains the requested HIT data.
    hit :: Prelude.Maybe HIT,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetHITResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hit', 'getHITResponse_hit' - Contains the requested HIT data.
--
-- 'httpStatus', 'getHITResponse_httpStatus' - The response's http status code.
newGetHITResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetHITResponse
newGetHITResponse pHttpStatus_ =
  GetHITResponse'
    { hit = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the requested HIT data.
getHITResponse_hit :: Lens.Lens' GetHITResponse (Prelude.Maybe HIT)
getHITResponse_hit = Lens.lens (\GetHITResponse' {hit} -> hit) (\s@GetHITResponse' {} a -> s {hit = a} :: GetHITResponse)

-- | The response's http status code.
getHITResponse_httpStatus :: Lens.Lens' GetHITResponse Prelude.Int
getHITResponse_httpStatus = Lens.lens (\GetHITResponse' {httpStatus} -> httpStatus) (\s@GetHITResponse' {} a -> s {httpStatus = a} :: GetHITResponse)

instance Prelude.NFData GetHITResponse where
  rnf GetHITResponse' {..} =
    Prelude.rnf hit
      `Prelude.seq` Prelude.rnf httpStatus
