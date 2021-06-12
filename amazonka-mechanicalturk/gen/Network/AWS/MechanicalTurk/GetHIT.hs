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
-- Module      : Network.AWS.MechanicalTurk.GetHIT
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetHIT@ operation retrieves the details of the specified HIT.
module Network.AWS.MechanicalTurk.GetHIT
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetHIT' smart constructor.
data GetHIT = GetHIT'
  { -- | The ID of the HIT to be retrieved.
    hITId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetHIT
newGetHIT pHITId_ = GetHIT' {hITId = pHITId_}

-- | The ID of the HIT to be retrieved.
getHIT_hITId :: Lens.Lens' GetHIT Core.Text
getHIT_hITId = Lens.lens (\GetHIT' {hITId} -> hITId) (\s@GetHIT' {} a -> s {hITId = a} :: GetHIT)

instance Core.AWSRequest GetHIT where
  type AWSResponse GetHIT = GetHITResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetHITResponse'
            Core.<$> (x Core..?> "HIT")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetHIT

instance Core.NFData GetHIT

instance Core.ToHeaders GetHIT where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.GetHIT" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetHIT where
  toJSON GetHIT' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("HITId" Core..= hITId)])

instance Core.ToPath GetHIT where
  toPath = Core.const "/"

instance Core.ToQuery GetHIT where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetHITResponse' smart constructor.
data GetHITResponse = GetHITResponse'
  { -- | Contains the requested HIT data.
    hit :: Core.Maybe HIT,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetHITResponse
newGetHITResponse pHttpStatus_ =
  GetHITResponse'
    { hit = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the requested HIT data.
getHITResponse_hit :: Lens.Lens' GetHITResponse (Core.Maybe HIT)
getHITResponse_hit = Lens.lens (\GetHITResponse' {hit} -> hit) (\s@GetHITResponse' {} a -> s {hit = a} :: GetHITResponse)

-- | The response's http status code.
getHITResponse_httpStatus :: Lens.Lens' GetHITResponse Core.Int
getHITResponse_httpStatus = Lens.lens (\GetHITResponse' {httpStatus} -> httpStatus) (\s@GetHITResponse' {} a -> s {httpStatus = a} :: GetHITResponse)

instance Core.NFData GetHITResponse
