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
-- Module      : Amazonka.SSMContacts.PutContactPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a resource policy to the specified contact or escalation plan. The
-- resource policy is used to share the contact or escalation plan using
-- Resource Access Manager (RAM). For more information about cross-account
-- sharing, see
-- <https://docs.aws.amazon.com/incident-manager/latest/userguide/xa.html Setting up cross-account functionality>.
module Amazonka.SSMContacts.PutContactPolicy
  ( -- * Creating a Request
    PutContactPolicy (..),
    newPutContactPolicy,

    -- * Request Lenses
    putContactPolicy_contactArn,
    putContactPolicy_policy,

    -- * Destructuring the Response
    PutContactPolicyResponse (..),
    newPutContactPolicyResponse,

    -- * Response Lenses
    putContactPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newPutContactPolicy' smart constructor.
data PutContactPolicy = PutContactPolicy'
  { -- | The Amazon Resource Name (ARN) of the contact or escalation plan.
    contactArn :: Prelude.Text,
    -- | Details of the resource policy.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutContactPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactArn', 'putContactPolicy_contactArn' - The Amazon Resource Name (ARN) of the contact or escalation plan.
--
-- 'policy', 'putContactPolicy_policy' - Details of the resource policy.
newPutContactPolicy ::
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutContactPolicy
newPutContactPolicy pContactArn_ pPolicy_ =
  PutContactPolicy'
    { contactArn = pContactArn_,
      policy = pPolicy_
    }

-- | The Amazon Resource Name (ARN) of the contact or escalation plan.
putContactPolicy_contactArn :: Lens.Lens' PutContactPolicy Prelude.Text
putContactPolicy_contactArn = Lens.lens (\PutContactPolicy' {contactArn} -> contactArn) (\s@PutContactPolicy' {} a -> s {contactArn = a} :: PutContactPolicy)

-- | Details of the resource policy.
putContactPolicy_policy :: Lens.Lens' PutContactPolicy Prelude.Text
putContactPolicy_policy = Lens.lens (\PutContactPolicy' {policy} -> policy) (\s@PutContactPolicy' {} a -> s {policy = a} :: PutContactPolicy)

instance Core.AWSRequest PutContactPolicy where
  type
    AWSResponse PutContactPolicy =
      PutContactPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutContactPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutContactPolicy where
  hashWithSalt _salt PutContactPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` contactArn
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutContactPolicy where
  rnf PutContactPolicy' {..} =
    Prelude.rnf contactArn
      `Prelude.seq` Prelude.rnf policy

instance Data.ToHeaders PutContactPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.PutContactPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutContactPolicy where
  toJSON PutContactPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ContactArn" Data..= contactArn),
            Prelude.Just ("Policy" Data..= policy)
          ]
      )

instance Data.ToPath PutContactPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutContactPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutContactPolicyResponse' smart constructor.
data PutContactPolicyResponse = PutContactPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutContactPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putContactPolicyResponse_httpStatus' - The response's http status code.
newPutContactPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutContactPolicyResponse
newPutContactPolicyResponse pHttpStatus_ =
  PutContactPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putContactPolicyResponse_httpStatus :: Lens.Lens' PutContactPolicyResponse Prelude.Int
putContactPolicyResponse_httpStatus = Lens.lens (\PutContactPolicyResponse' {httpStatus} -> httpStatus) (\s@PutContactPolicyResponse' {} a -> s {httpStatus = a} :: PutContactPolicyResponse)

instance Prelude.NFData PutContactPolicyResponse where
  rnf PutContactPolicyResponse' {..} =
    Prelude.rnf httpStatus
