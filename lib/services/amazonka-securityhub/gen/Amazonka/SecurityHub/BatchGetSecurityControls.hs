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
-- Module      : Amazonka.SecurityHub.BatchGetSecurityControls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about a batch of security controls for the current
-- Amazon Web Services account and Amazon Web Services Region.
module Amazonka.SecurityHub.BatchGetSecurityControls
  ( -- * Creating a Request
    BatchGetSecurityControls (..),
    newBatchGetSecurityControls,

    -- * Request Lenses
    batchGetSecurityControls_securityControlIds,

    -- * Destructuring the Response
    BatchGetSecurityControlsResponse (..),
    newBatchGetSecurityControlsResponse,

    -- * Response Lenses
    batchGetSecurityControlsResponse_unprocessedIds,
    batchGetSecurityControlsResponse_httpStatus,
    batchGetSecurityControlsResponse_securityControls,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newBatchGetSecurityControls' smart constructor.
data BatchGetSecurityControls = BatchGetSecurityControls'
  { -- | A list of security controls (identified with @SecurityControlId@,
    -- @SecurityControlArn@, or a mix of both parameters). The security control
    -- ID or Amazon Resource Name (ARN) is the same across standards.
    securityControlIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetSecurityControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityControlIds', 'batchGetSecurityControls_securityControlIds' - A list of security controls (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters). The security control
-- ID or Amazon Resource Name (ARN) is the same across standards.
newBatchGetSecurityControls ::
  BatchGetSecurityControls
newBatchGetSecurityControls =
  BatchGetSecurityControls'
    { securityControlIds =
        Prelude.mempty
    }

-- | A list of security controls (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters). The security control
-- ID or Amazon Resource Name (ARN) is the same across standards.
batchGetSecurityControls_securityControlIds :: Lens.Lens' BatchGetSecurityControls [Prelude.Text]
batchGetSecurityControls_securityControlIds = Lens.lens (\BatchGetSecurityControls' {securityControlIds} -> securityControlIds) (\s@BatchGetSecurityControls' {} a -> s {securityControlIds = a} :: BatchGetSecurityControls) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetSecurityControls where
  type
    AWSResponse BatchGetSecurityControls =
      BatchGetSecurityControlsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetSecurityControlsResponse'
            Prelude.<$> (x Data..?> "UnprocessedIds" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "SecurityControls"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable BatchGetSecurityControls where
  hashWithSalt _salt BatchGetSecurityControls' {..} =
    _salt `Prelude.hashWithSalt` securityControlIds

instance Prelude.NFData BatchGetSecurityControls where
  rnf BatchGetSecurityControls' {..} =
    Prelude.rnf securityControlIds

instance Data.ToHeaders BatchGetSecurityControls where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetSecurityControls where
  toJSON BatchGetSecurityControls' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SecurityControlIds" Data..= securityControlIds)
          ]
      )

instance Data.ToPath BatchGetSecurityControls where
  toPath = Prelude.const "/securityControls/batchGet"

instance Data.ToQuery BatchGetSecurityControls where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetSecurityControlsResponse' smart constructor.
data BatchGetSecurityControlsResponse = BatchGetSecurityControlsResponse'
  { -- | A security control (identified with @SecurityControlId@,
    -- @SecurityControlArn@, or a mix of both parameters) for which details
    -- cannot be returned.
    unprocessedIds :: Prelude.Maybe [UnprocessedSecurityControl],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array that returns the identifier, Amazon Resource Name (ARN), and
    -- other details about a security control. The same information is returned
    -- whether the request includes @SecurityControlId@ or
    -- @SecurityControlArn@.
    securityControls :: [SecurityControl]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetSecurityControlsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unprocessedIds', 'batchGetSecurityControlsResponse_unprocessedIds' - A security control (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters) for which details
-- cannot be returned.
--
-- 'httpStatus', 'batchGetSecurityControlsResponse_httpStatus' - The response's http status code.
--
-- 'securityControls', 'batchGetSecurityControlsResponse_securityControls' - An array that returns the identifier, Amazon Resource Name (ARN), and
-- other details about a security control. The same information is returned
-- whether the request includes @SecurityControlId@ or
-- @SecurityControlArn@.
newBatchGetSecurityControlsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetSecurityControlsResponse
newBatchGetSecurityControlsResponse pHttpStatus_ =
  BatchGetSecurityControlsResponse'
    { unprocessedIds =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      securityControls = Prelude.mempty
    }

-- | A security control (identified with @SecurityControlId@,
-- @SecurityControlArn@, or a mix of both parameters) for which details
-- cannot be returned.
batchGetSecurityControlsResponse_unprocessedIds :: Lens.Lens' BatchGetSecurityControlsResponse (Prelude.Maybe [UnprocessedSecurityControl])
batchGetSecurityControlsResponse_unprocessedIds = Lens.lens (\BatchGetSecurityControlsResponse' {unprocessedIds} -> unprocessedIds) (\s@BatchGetSecurityControlsResponse' {} a -> s {unprocessedIds = a} :: BatchGetSecurityControlsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetSecurityControlsResponse_httpStatus :: Lens.Lens' BatchGetSecurityControlsResponse Prelude.Int
batchGetSecurityControlsResponse_httpStatus = Lens.lens (\BatchGetSecurityControlsResponse' {httpStatus} -> httpStatus) (\s@BatchGetSecurityControlsResponse' {} a -> s {httpStatus = a} :: BatchGetSecurityControlsResponse)

-- | An array that returns the identifier, Amazon Resource Name (ARN), and
-- other details about a security control. The same information is returned
-- whether the request includes @SecurityControlId@ or
-- @SecurityControlArn@.
batchGetSecurityControlsResponse_securityControls :: Lens.Lens' BatchGetSecurityControlsResponse [SecurityControl]
batchGetSecurityControlsResponse_securityControls = Lens.lens (\BatchGetSecurityControlsResponse' {securityControls} -> securityControls) (\s@BatchGetSecurityControlsResponse' {} a -> s {securityControls = a} :: BatchGetSecurityControlsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    BatchGetSecurityControlsResponse
  where
  rnf BatchGetSecurityControlsResponse' {..} =
    Prelude.rnf unprocessedIds
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf securityControls
