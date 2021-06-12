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
-- Module      : Network.AWS.DirectoryService.VerifyTrust
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- AWS Directory Service for Microsoft Active Directory allows you to
-- configure and verify trust relationships.
--
-- This action verifies a trust relationship between your AWS Managed
-- Microsoft AD directory and an external domain.
module Network.AWS.DirectoryService.VerifyTrust
  ( -- * Creating a Request
    VerifyTrust (..),
    newVerifyTrust,

    -- * Request Lenses
    verifyTrust_trustId,

    -- * Destructuring the Response
    VerifyTrustResponse (..),
    newVerifyTrustResponse,

    -- * Response Lenses
    verifyTrustResponse_trustId,
    verifyTrustResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Initiates the verification of an existing trust relationship between an
-- AWS Managed Microsoft AD directory and an external domain.
--
-- /See:/ 'newVerifyTrust' smart constructor.
data VerifyTrust = VerifyTrust'
  { -- | The unique Trust ID of the trust relationship to verify.
    trustId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VerifyTrust' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustId', 'verifyTrust_trustId' - The unique Trust ID of the trust relationship to verify.
newVerifyTrust ::
  -- | 'trustId'
  Core.Text ->
  VerifyTrust
newVerifyTrust pTrustId_ =
  VerifyTrust' {trustId = pTrustId_}

-- | The unique Trust ID of the trust relationship to verify.
verifyTrust_trustId :: Lens.Lens' VerifyTrust Core.Text
verifyTrust_trustId = Lens.lens (\VerifyTrust' {trustId} -> trustId) (\s@VerifyTrust' {} a -> s {trustId = a} :: VerifyTrust)

instance Core.AWSRequest VerifyTrust where
  type AWSResponse VerifyTrust = VerifyTrustResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifyTrustResponse'
            Core.<$> (x Core..?> "TrustId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable VerifyTrust

instance Core.NFData VerifyTrust

instance Core.ToHeaders VerifyTrust where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.VerifyTrust" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON VerifyTrust where
  toJSON VerifyTrust' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("TrustId" Core..= trustId)]
      )

instance Core.ToPath VerifyTrust where
  toPath = Core.const "/"

instance Core.ToQuery VerifyTrust where
  toQuery = Core.const Core.mempty

-- | Result of a VerifyTrust request.
--
-- /See:/ 'newVerifyTrustResponse' smart constructor.
data VerifyTrustResponse = VerifyTrustResponse'
  { -- | The unique Trust ID of the trust relationship that was verified.
    trustId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VerifyTrustResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trustId', 'verifyTrustResponse_trustId' - The unique Trust ID of the trust relationship that was verified.
--
-- 'httpStatus', 'verifyTrustResponse_httpStatus' - The response's http status code.
newVerifyTrustResponse ::
  -- | 'httpStatus'
  Core.Int ->
  VerifyTrustResponse
newVerifyTrustResponse pHttpStatus_ =
  VerifyTrustResponse'
    { trustId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique Trust ID of the trust relationship that was verified.
verifyTrustResponse_trustId :: Lens.Lens' VerifyTrustResponse (Core.Maybe Core.Text)
verifyTrustResponse_trustId = Lens.lens (\VerifyTrustResponse' {trustId} -> trustId) (\s@VerifyTrustResponse' {} a -> s {trustId = a} :: VerifyTrustResponse)

-- | The response's http status code.
verifyTrustResponse_httpStatus :: Lens.Lens' VerifyTrustResponse Core.Int
verifyTrustResponse_httpStatus = Lens.lens (\VerifyTrustResponse' {httpStatus} -> httpStatus) (\s@VerifyTrustResponse' {} a -> s {httpStatus = a} :: VerifyTrustResponse)

instance Core.NFData VerifyTrustResponse
