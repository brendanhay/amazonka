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
-- Module      : Amazonka.DirectoryService.VerifyTrust
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Directory Service for Microsoft Active Directory allows you to configure
-- and verify trust relationships.
--
-- This action verifies a trust relationship between your Managed Microsoft
-- AD directory and an external domain.
module Amazonka.DirectoryService.VerifyTrust
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Initiates the verification of an existing trust relationship between an
-- Managed Microsoft AD directory and an external domain.
--
-- /See:/ 'newVerifyTrust' smart constructor.
data VerifyTrust = VerifyTrust'
  { -- | The unique Trust ID of the trust relationship to verify.
    trustId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest VerifyTrust where
  type AWSResponse VerifyTrust = VerifyTrustResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          VerifyTrustResponse'
            Prelude.<$> (x Data..?> "TrustId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable VerifyTrust where
  hashWithSalt _salt VerifyTrust' {..} =
    _salt `Prelude.hashWithSalt` trustId

instance Prelude.NFData VerifyTrust where
  rnf VerifyTrust' {..} = Prelude.rnf trustId

instance Data.ToHeaders VerifyTrust where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.VerifyTrust" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON VerifyTrust where
  toJSON VerifyTrust' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("TrustId" Data..= trustId)]
      )

instance Data.ToPath VerifyTrust where
  toPath = Prelude.const "/"

instance Data.ToQuery VerifyTrust where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData VerifyTrustResponse where
  rnf VerifyTrustResponse' {..} =
    Prelude.rnf trustId
      `Prelude.seq` Prelude.rnf httpStatus
