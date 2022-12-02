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
-- Module      : Amazonka.ImageBuilder.PutComponentPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a policy to a component. We recommend that you call the RAM API
-- <https://docs.aws.amazon.com/ram/latest/APIReference/API_CreateResourceShare.html CreateResourceShare>
-- to share resources. If you call the Image Builder API
-- @PutComponentPolicy@, you must also call the RAM API
-- <https://docs.aws.amazon.com/ram/latest/APIReference/API_PromoteResourceShareCreatedFromPolicy.html PromoteResourceShareCreatedFromPolicy>
-- in order for the resource to be visible to all principals with whom the
-- resource is shared.
module Amazonka.ImageBuilder.PutComponentPolicy
  ( -- * Creating a Request
    PutComponentPolicy (..),
    newPutComponentPolicy,

    -- * Request Lenses
    putComponentPolicy_componentArn,
    putComponentPolicy_policy,

    -- * Destructuring the Response
    PutComponentPolicyResponse (..),
    newPutComponentPolicyResponse,

    -- * Response Lenses
    putComponentPolicyResponse_componentArn,
    putComponentPolicyResponse_requestId,
    putComponentPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutComponentPolicy' smart constructor.
data PutComponentPolicy = PutComponentPolicy'
  { -- | The Amazon Resource Name (ARN) of the component that this policy should
    -- be applied to.
    componentArn :: Prelude.Text,
    -- | The policy to apply.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutComponentPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentArn', 'putComponentPolicy_componentArn' - The Amazon Resource Name (ARN) of the component that this policy should
-- be applied to.
--
-- 'policy', 'putComponentPolicy_policy' - The policy to apply.
newPutComponentPolicy ::
  -- | 'componentArn'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutComponentPolicy
newPutComponentPolicy pComponentArn_ pPolicy_ =
  PutComponentPolicy'
    { componentArn = pComponentArn_,
      policy = pPolicy_
    }

-- | The Amazon Resource Name (ARN) of the component that this policy should
-- be applied to.
putComponentPolicy_componentArn :: Lens.Lens' PutComponentPolicy Prelude.Text
putComponentPolicy_componentArn = Lens.lens (\PutComponentPolicy' {componentArn} -> componentArn) (\s@PutComponentPolicy' {} a -> s {componentArn = a} :: PutComponentPolicy)

-- | The policy to apply.
putComponentPolicy_policy :: Lens.Lens' PutComponentPolicy Prelude.Text
putComponentPolicy_policy = Lens.lens (\PutComponentPolicy' {policy} -> policy) (\s@PutComponentPolicy' {} a -> s {policy = a} :: PutComponentPolicy)

instance Core.AWSRequest PutComponentPolicy where
  type
    AWSResponse PutComponentPolicy =
      PutComponentPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutComponentPolicyResponse'
            Prelude.<$> (x Data..?> "componentArn")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutComponentPolicy where
  hashWithSalt _salt PutComponentPolicy' {..} =
    _salt `Prelude.hashWithSalt` componentArn
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutComponentPolicy where
  rnf PutComponentPolicy' {..} =
    Prelude.rnf componentArn
      `Prelude.seq` Prelude.rnf policy

instance Data.ToHeaders PutComponentPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutComponentPolicy where
  toJSON PutComponentPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("componentArn" Data..= componentArn),
            Prelude.Just ("policy" Data..= policy)
          ]
      )

instance Data.ToPath PutComponentPolicy where
  toPath = Prelude.const "/PutComponentPolicy"

instance Data.ToQuery PutComponentPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutComponentPolicyResponse' smart constructor.
data PutComponentPolicyResponse = PutComponentPolicyResponse'
  { -- | The Amazon Resource Name (ARN) of the component that this policy was
    -- applied to.
    componentArn :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutComponentPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentArn', 'putComponentPolicyResponse_componentArn' - The Amazon Resource Name (ARN) of the component that this policy was
-- applied to.
--
-- 'requestId', 'putComponentPolicyResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'httpStatus', 'putComponentPolicyResponse_httpStatus' - The response's http status code.
newPutComponentPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutComponentPolicyResponse
newPutComponentPolicyResponse pHttpStatus_ =
  PutComponentPolicyResponse'
    { componentArn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the component that this policy was
-- applied to.
putComponentPolicyResponse_componentArn :: Lens.Lens' PutComponentPolicyResponse (Prelude.Maybe Prelude.Text)
putComponentPolicyResponse_componentArn = Lens.lens (\PutComponentPolicyResponse' {componentArn} -> componentArn) (\s@PutComponentPolicyResponse' {} a -> s {componentArn = a} :: PutComponentPolicyResponse)

-- | The request ID that uniquely identifies this request.
putComponentPolicyResponse_requestId :: Lens.Lens' PutComponentPolicyResponse (Prelude.Maybe Prelude.Text)
putComponentPolicyResponse_requestId = Lens.lens (\PutComponentPolicyResponse' {requestId} -> requestId) (\s@PutComponentPolicyResponse' {} a -> s {requestId = a} :: PutComponentPolicyResponse)

-- | The response's http status code.
putComponentPolicyResponse_httpStatus :: Lens.Lens' PutComponentPolicyResponse Prelude.Int
putComponentPolicyResponse_httpStatus = Lens.lens (\PutComponentPolicyResponse' {httpStatus} -> httpStatus) (\s@PutComponentPolicyResponse' {} a -> s {httpStatus = a} :: PutComponentPolicyResponse)

instance Prelude.NFData PutComponentPolicyResponse where
  rnf PutComponentPolicyResponse' {..} =
    Prelude.rnf componentArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf httpStatus
