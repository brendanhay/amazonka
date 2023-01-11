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
-- Module      : Amazonka.MediaConvert.PutPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create or change your policy. For more information about policies, see
-- the user guide at
-- http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
module Amazonka.MediaConvert.PutPolicy
  ( -- * Creating a Request
    PutPolicy (..),
    newPutPolicy,

    -- * Request Lenses
    putPolicy_policy,

    -- * Destructuring the Response
    PutPolicyResponse (..),
    newPutPolicyResponse,

    -- * Response Lenses
    putPolicyResponse_policy,
    putPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutPolicy' smart constructor.
data PutPolicy = PutPolicy'
  { -- | A policy configures behavior that you allow or disallow for your
    -- account. For information about MediaConvert policies, see the user guide
    -- at http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
    policy :: Policy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putPolicy_policy' - A policy configures behavior that you allow or disallow for your
-- account. For information about MediaConvert policies, see the user guide
-- at http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
newPutPolicy ::
  -- | 'policy'
  Policy ->
  PutPolicy
newPutPolicy pPolicy_ = PutPolicy' {policy = pPolicy_}

-- | A policy configures behavior that you allow or disallow for your
-- account. For information about MediaConvert policies, see the user guide
-- at http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
putPolicy_policy :: Lens.Lens' PutPolicy Policy
putPolicy_policy = Lens.lens (\PutPolicy' {policy} -> policy) (\s@PutPolicy' {} a -> s {policy = a} :: PutPolicy)

instance Core.AWSRequest PutPolicy where
  type AWSResponse PutPolicy = PutPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutPolicyResponse'
            Prelude.<$> (x Data..?> "policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutPolicy where
  hashWithSalt _salt PutPolicy' {..} =
    _salt `Prelude.hashWithSalt` policy

instance Prelude.NFData PutPolicy where
  rnf PutPolicy' {..} = Prelude.rnf policy

instance Data.ToHeaders PutPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutPolicy where
  toJSON PutPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("policy" Data..= policy)]
      )

instance Data.ToPath PutPolicy where
  toPath = Prelude.const "/2017-08-29/policy"

instance Data.ToQuery PutPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutPolicyResponse' smart constructor.
data PutPolicyResponse = PutPolicyResponse'
  { -- | A policy configures behavior that you allow or disallow for your
    -- account. For information about MediaConvert policies, see the user guide
    -- at http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
    policy :: Prelude.Maybe Policy,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putPolicyResponse_policy' - A policy configures behavior that you allow or disallow for your
-- account. For information about MediaConvert policies, see the user guide
-- at http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
--
-- 'httpStatus', 'putPolicyResponse_httpStatus' - The response's http status code.
newPutPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutPolicyResponse
newPutPolicyResponse pHttpStatus_ =
  PutPolicyResponse'
    { policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A policy configures behavior that you allow or disallow for your
-- account. For information about MediaConvert policies, see the user guide
-- at http:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/what-is.html
putPolicyResponse_policy :: Lens.Lens' PutPolicyResponse (Prelude.Maybe Policy)
putPolicyResponse_policy = Lens.lens (\PutPolicyResponse' {policy} -> policy) (\s@PutPolicyResponse' {} a -> s {policy = a} :: PutPolicyResponse)

-- | The response's http status code.
putPolicyResponse_httpStatus :: Lens.Lens' PutPolicyResponse Prelude.Int
putPolicyResponse_httpStatus = Lens.lens (\PutPolicyResponse' {httpStatus} -> httpStatus) (\s@PutPolicyResponse' {} a -> s {httpStatus = a} :: PutPolicyResponse)

instance Prelude.NFData PutPolicyResponse where
  rnf PutPolicyResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
