{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Initiates the verification of an existing trust relationship between an
-- AWS Managed Microsoft AD directory and an external domain.
--
-- /See:/ 'newVerifyTrust' smart constructor.
data VerifyTrust = VerifyTrust'
  { -- | The unique Trust ID of the trust relationship to verify.
    trustId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  VerifyTrust
newVerifyTrust pTrustId_ =
  VerifyTrust' {trustId = pTrustId_}

-- | The unique Trust ID of the trust relationship to verify.
verifyTrust_trustId :: Lens.Lens' VerifyTrust Prelude.Text
verifyTrust_trustId = Lens.lens (\VerifyTrust' {trustId} -> trustId) (\s@VerifyTrust' {} a -> s {trustId = a} :: VerifyTrust)

instance Prelude.AWSRequest VerifyTrust where
  type Rs VerifyTrust = VerifyTrustResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifyTrustResponse'
            Prelude.<$> (x Prelude..?> "TrustId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable VerifyTrust

instance Prelude.NFData VerifyTrust

instance Prelude.ToHeaders VerifyTrust where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.VerifyTrust" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON VerifyTrust where
  toJSON VerifyTrust' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("TrustId" Prelude..= trustId)]
      )

instance Prelude.ToPath VerifyTrust where
  toPath = Prelude.const "/"

instance Prelude.ToQuery VerifyTrust where
  toQuery = Prelude.const Prelude.mempty

-- | Result of a VerifyTrust request.
--
-- /See:/ 'newVerifyTrustResponse' smart constructor.
data VerifyTrustResponse = VerifyTrustResponse'
  { -- | The unique Trust ID of the trust relationship that was verified.
    trustId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  VerifyTrustResponse
newVerifyTrustResponse pHttpStatus_ =
  VerifyTrustResponse'
    { trustId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique Trust ID of the trust relationship that was verified.
verifyTrustResponse_trustId :: Lens.Lens' VerifyTrustResponse (Prelude.Maybe Prelude.Text)
verifyTrustResponse_trustId = Lens.lens (\VerifyTrustResponse' {trustId} -> trustId) (\s@VerifyTrustResponse' {} a -> s {trustId = a} :: VerifyTrustResponse)

-- | The response's http status code.
verifyTrustResponse_httpStatus :: Lens.Lens' VerifyTrustResponse Prelude.Int
verifyTrustResponse_httpStatus = Lens.lens (\VerifyTrustResponse' {httpStatus} -> httpStatus) (\s@VerifyTrustResponse' {} a -> s {httpStatus = a} :: VerifyTrustResponse)

instance Prelude.NFData VerifyTrustResponse
