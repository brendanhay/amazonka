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
-- Module      : Amazonka.OAM.PutSinkPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates the resource policy that grants permissions to source
-- accounts to link to the monitoring account sink. When you create a sink
-- policy, you can grant permissions to all accounts in an organization or
-- to individual accounts.
--
-- You can also use a sink policy to limit the types of data that is
-- shared. The three types that you can allow or deny are:
--
-- -   __Metrics__ - Specify with @AWS::CloudWatch::Metric@
--
-- -   __Log groups__ - Specify with @AWS::Logs::LogGroup@
--
-- -   __Traces__ - Specify with @AWS::XRay::Trace@
--
-- See the examples in this section to see how to specify permitted source
-- accounts and data types.
module Amazonka.OAM.PutSinkPolicy
  ( -- * Creating a Request
    PutSinkPolicy (..),
    newPutSinkPolicy,

    -- * Request Lenses
    putSinkPolicy_sinkIdentifier,
    putSinkPolicy_policy,

    -- * Destructuring the Response
    PutSinkPolicyResponse (..),
    newPutSinkPolicyResponse,

    -- * Response Lenses
    putSinkPolicyResponse_policy,
    putSinkPolicyResponse_sinkArn,
    putSinkPolicyResponse_sinkId,
    putSinkPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutSinkPolicy' smart constructor.
data PutSinkPolicy = PutSinkPolicy'
  { -- | The ARN of the sink to attach this policy to.
    sinkIdentifier :: Prelude.Text,
    -- | The JSON policy to use. If you are updating an existing policy, the
    -- entire existing policy is replaced by what you specify here.
    --
    -- The policy must be in JSON string format with quotation marks escaped
    -- and no newlines.
    --
    -- For examples of different types of policies, see the __Examples__
    -- section on this page.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutSinkPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sinkIdentifier', 'putSinkPolicy_sinkIdentifier' - The ARN of the sink to attach this policy to.
--
-- 'policy', 'putSinkPolicy_policy' - The JSON policy to use. If you are updating an existing policy, the
-- entire existing policy is replaced by what you specify here.
--
-- The policy must be in JSON string format with quotation marks escaped
-- and no newlines.
--
-- For examples of different types of policies, see the __Examples__
-- section on this page.
newPutSinkPolicy ::
  -- | 'sinkIdentifier'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutSinkPolicy
newPutSinkPolicy pSinkIdentifier_ pPolicy_ =
  PutSinkPolicy'
    { sinkIdentifier = pSinkIdentifier_,
      policy = pPolicy_
    }

-- | The ARN of the sink to attach this policy to.
putSinkPolicy_sinkIdentifier :: Lens.Lens' PutSinkPolicy Prelude.Text
putSinkPolicy_sinkIdentifier = Lens.lens (\PutSinkPolicy' {sinkIdentifier} -> sinkIdentifier) (\s@PutSinkPolicy' {} a -> s {sinkIdentifier = a} :: PutSinkPolicy)

-- | The JSON policy to use. If you are updating an existing policy, the
-- entire existing policy is replaced by what you specify here.
--
-- The policy must be in JSON string format with quotation marks escaped
-- and no newlines.
--
-- For examples of different types of policies, see the __Examples__
-- section on this page.
putSinkPolicy_policy :: Lens.Lens' PutSinkPolicy Prelude.Text
putSinkPolicy_policy = Lens.lens (\PutSinkPolicy' {policy} -> policy) (\s@PutSinkPolicy' {} a -> s {policy = a} :: PutSinkPolicy)

instance Core.AWSRequest PutSinkPolicy where
  type
    AWSResponse PutSinkPolicy =
      PutSinkPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutSinkPolicyResponse'
            Prelude.<$> (x Data..?> "Policy")
            Prelude.<*> (x Data..?> "SinkArn")
            Prelude.<*> (x Data..?> "SinkId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutSinkPolicy where
  hashWithSalt _salt PutSinkPolicy' {..} =
    _salt `Prelude.hashWithSalt` sinkIdentifier
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutSinkPolicy where
  rnf PutSinkPolicy' {..} =
    Prelude.rnf sinkIdentifier
      `Prelude.seq` Prelude.rnf policy

instance Data.ToHeaders PutSinkPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutSinkPolicy where
  toJSON PutSinkPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("SinkIdentifier" Data..= sinkIdentifier),
            Prelude.Just ("Policy" Data..= policy)
          ]
      )

instance Data.ToPath PutSinkPolicy where
  toPath = Prelude.const "/PutSinkPolicy"

instance Data.ToQuery PutSinkPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutSinkPolicyResponse' smart constructor.
data PutSinkPolicyResponse = PutSinkPolicyResponse'
  { -- | The policy that you specified.
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
-- Create a value of 'PutSinkPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putSinkPolicyResponse_policy' - The policy that you specified.
--
-- 'sinkArn', 'putSinkPolicyResponse_sinkArn' - The ARN of the sink.
--
-- 'sinkId', 'putSinkPolicyResponse_sinkId' - The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
--
-- 'httpStatus', 'putSinkPolicyResponse_httpStatus' - The response's http status code.
newPutSinkPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutSinkPolicyResponse
newPutSinkPolicyResponse pHttpStatus_ =
  PutSinkPolicyResponse'
    { policy = Prelude.Nothing,
      sinkArn = Prelude.Nothing,
      sinkId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy that you specified.
putSinkPolicyResponse_policy :: Lens.Lens' PutSinkPolicyResponse (Prelude.Maybe Prelude.Text)
putSinkPolicyResponse_policy = Lens.lens (\PutSinkPolicyResponse' {policy} -> policy) (\s@PutSinkPolicyResponse' {} a -> s {policy = a} :: PutSinkPolicyResponse)

-- | The ARN of the sink.
putSinkPolicyResponse_sinkArn :: Lens.Lens' PutSinkPolicyResponse (Prelude.Maybe Prelude.Text)
putSinkPolicyResponse_sinkArn = Lens.lens (\PutSinkPolicyResponse' {sinkArn} -> sinkArn) (\s@PutSinkPolicyResponse' {} a -> s {sinkArn = a} :: PutSinkPolicyResponse)

-- | The random ID string that Amazon Web Services generated as part of the
-- sink ARN.
putSinkPolicyResponse_sinkId :: Lens.Lens' PutSinkPolicyResponse (Prelude.Maybe Prelude.Text)
putSinkPolicyResponse_sinkId = Lens.lens (\PutSinkPolicyResponse' {sinkId} -> sinkId) (\s@PutSinkPolicyResponse' {} a -> s {sinkId = a} :: PutSinkPolicyResponse)

-- | The response's http status code.
putSinkPolicyResponse_httpStatus :: Lens.Lens' PutSinkPolicyResponse Prelude.Int
putSinkPolicyResponse_httpStatus = Lens.lens (\PutSinkPolicyResponse' {httpStatus} -> httpStatus) (\s@PutSinkPolicyResponse' {} a -> s {httpStatus = a} :: PutSinkPolicyResponse)

instance Prelude.NFData PutSinkPolicyResponse where
  rnf PutSinkPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf sinkArn
      `Prelude.seq` Prelude.rnf sinkId
      `Prelude.seq` Prelude.rnf httpStatus
