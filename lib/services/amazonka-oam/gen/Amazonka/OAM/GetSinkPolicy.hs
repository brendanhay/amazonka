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
-- Module      : Amazonka.OAM.GetSinkPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current sink policy attached to this sink. The sink policy
-- specifies what accounts can attach to this sink as source accounts, and
-- what types of data they can share.
module Amazonka.OAM.GetSinkPolicy
  ( -- * Creating a Request
    GetSinkPolicy (..),
    newGetSinkPolicy,

    -- * Request Lenses
    getSinkPolicy_sinkIdentifier,

    -- * Destructuring the Response
    GetSinkPolicyResponse (..),
    newGetSinkPolicyResponse,

    -- * Response Lenses
    getSinkPolicyResponse_policy,
    getSinkPolicyResponse_sinkArn,
    getSinkPolicyResponse_sinkId,
    getSinkPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSinkPolicy' smart constructor.
data GetSinkPolicy = GetSinkPolicy'
  { -- | The ARN of the sink to retrieve the policy of.
    sinkIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSinkPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sinkIdentifier', 'getSinkPolicy_sinkIdentifier' - The ARN of the sink to retrieve the policy of.
newGetSinkPolicy ::
  -- | 'sinkIdentifier'
  Prelude.Text ->
  GetSinkPolicy
newGetSinkPolicy pSinkIdentifier_ =
  GetSinkPolicy' {sinkIdentifier = pSinkIdentifier_}

-- | The ARN of the sink to retrieve the policy of.
getSinkPolicy_sinkIdentifier :: Lens.Lens' GetSinkPolicy Prelude.Text
getSinkPolicy_sinkIdentifier = Lens.lens (\GetSinkPolicy' {sinkIdentifier} -> sinkIdentifier) (\s@GetSinkPolicy' {} a -> s {sinkIdentifier = a} :: GetSinkPolicy)

instance Core.AWSRequest GetSinkPolicy where
  type
    AWSResponse GetSinkPolicy =
      GetSinkPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSinkPolicyResponse'
            Prelude.<$> (x Data..?> "Policy")
            Prelude.<*> (x Data..?> "SinkArn")
            Prelude.<*> (x Data..?> "SinkId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSinkPolicy where
  hashWithSalt _salt GetSinkPolicy' {..} =
    _salt `Prelude.hashWithSalt` sinkIdentifier

instance Prelude.NFData GetSinkPolicy where
  rnf GetSinkPolicy' {..} = Prelude.rnf sinkIdentifier

instance Data.ToHeaders GetSinkPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSinkPolicy where
  toJSON GetSinkPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SinkIdentifier" Data..= sinkIdentifier)
          ]
      )

instance Data.ToPath GetSinkPolicy where
  toPath = Prelude.const "/GetSinkPolicy"

instance Data.ToQuery GetSinkPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSinkPolicyResponse' smart constructor.
data GetSinkPolicyResponse = GetSinkPolicyResponse'
  { -- | The policy that you specified, in JSON format.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the sink.
    sinkArn :: Prelude.Maybe Prelude.Text,
    -- | The random ID string that Amazon Web Services generated as part of the
    -- sink ARN.
    sinkId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSinkPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getSinkPolicyResponse_policy' - The policy that you specified, in JSON format.
--
-- 'sinkArn', 'getSinkPolicyResponse_sinkArn' - The ARN of the sink.
--
-- 'sinkId', 'getSinkPolicyResponse_sinkId' - The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
--
-- 'httpStatus', 'getSinkPolicyResponse_httpStatus' - The response's http status code.
newGetSinkPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSinkPolicyResponse
newGetSinkPolicyResponse pHttpStatus_ =
  GetSinkPolicyResponse'
    { policy = Prelude.Nothing,
      sinkArn = Prelude.Nothing,
      sinkId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy that you specified, in JSON format.
getSinkPolicyResponse_policy :: Lens.Lens' GetSinkPolicyResponse (Prelude.Maybe Prelude.Text)
getSinkPolicyResponse_policy = Lens.lens (\GetSinkPolicyResponse' {policy} -> policy) (\s@GetSinkPolicyResponse' {} a -> s {policy = a} :: GetSinkPolicyResponse)

-- | The ARN of the sink.
getSinkPolicyResponse_sinkArn :: Lens.Lens' GetSinkPolicyResponse (Prelude.Maybe Prelude.Text)
getSinkPolicyResponse_sinkArn = Lens.lens (\GetSinkPolicyResponse' {sinkArn} -> sinkArn) (\s@GetSinkPolicyResponse' {} a -> s {sinkArn = a} :: GetSinkPolicyResponse)

-- | The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
getSinkPolicyResponse_sinkId :: Lens.Lens' GetSinkPolicyResponse (Prelude.Maybe Prelude.Text)
getSinkPolicyResponse_sinkId = Lens.lens (\GetSinkPolicyResponse' {sinkId} -> sinkId) (\s@GetSinkPolicyResponse' {} a -> s {sinkId = a} :: GetSinkPolicyResponse)

-- | The response's http status code.
getSinkPolicyResponse_httpStatus :: Lens.Lens' GetSinkPolicyResponse Prelude.Int
getSinkPolicyResponse_httpStatus = Lens.lens (\GetSinkPolicyResponse' {httpStatus} -> httpStatus) (\s@GetSinkPolicyResponse' {} a -> s {httpStatus = a} :: GetSinkPolicyResponse)

instance Prelude.NFData GetSinkPolicyResponse where
  rnf GetSinkPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf sinkArn
      `Prelude.seq` Prelude.rnf sinkId
      `Prelude.seq` Prelude.rnf httpStatus
