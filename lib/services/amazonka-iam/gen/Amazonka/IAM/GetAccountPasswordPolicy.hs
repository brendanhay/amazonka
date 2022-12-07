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
-- Module      : Amazonka.IAM.GetAccountPasswordPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the password policy for the Amazon Web Services account. This
-- tells you the complexity requirements and mandatory rotation periods for
-- the IAM user passwords in your account. For more information about using
-- a password policy, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingPasswordPolicies.html Managing an IAM password policy>.
module Amazonka.IAM.GetAccountPasswordPolicy
  ( -- * Creating a Request
    GetAccountPasswordPolicy (..),
    newGetAccountPasswordPolicy,

    -- * Destructuring the Response
    GetAccountPasswordPolicyResponse (..),
    newGetAccountPasswordPolicyResponse,

    -- * Response Lenses
    getAccountPasswordPolicyResponse_httpStatus,
    getAccountPasswordPolicyResponse_passwordPolicy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccountPasswordPolicy' smart constructor.
data GetAccountPasswordPolicy = GetAccountPasswordPolicy'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountPasswordPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetAccountPasswordPolicy ::
  GetAccountPasswordPolicy
newGetAccountPasswordPolicy =
  GetAccountPasswordPolicy'

instance Core.AWSRequest GetAccountPasswordPolicy where
  type
    AWSResponse GetAccountPasswordPolicy =
      GetAccountPasswordPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetAccountPasswordPolicyResult"
      ( \s h x ->
          GetAccountPasswordPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "PasswordPolicy")
      )

instance Prelude.Hashable GetAccountPasswordPolicy where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetAccountPasswordPolicy where
  rnf _ = ()

instance Data.ToHeaders GetAccountPasswordPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetAccountPasswordPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAccountPasswordPolicy where
  toQuery =
    Prelude.const
      ( Prelude.mconcat
          [ "Action"
              Data.=: ("GetAccountPasswordPolicy" :: Prelude.ByteString),
            "Version"
              Data.=: ("2010-05-08" :: Prelude.ByteString)
          ]
      )

-- | Contains the response to a successful GetAccountPasswordPolicy request.
--
-- /See:/ 'newGetAccountPasswordPolicyResponse' smart constructor.
data GetAccountPasswordPolicyResponse = GetAccountPasswordPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A structure that contains details about the account\'s password policy.
    passwordPolicy :: PasswordPolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccountPasswordPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getAccountPasswordPolicyResponse_httpStatus' - The response's http status code.
--
-- 'passwordPolicy', 'getAccountPasswordPolicyResponse_passwordPolicy' - A structure that contains details about the account\'s password policy.
newGetAccountPasswordPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'passwordPolicy'
  PasswordPolicy ->
  GetAccountPasswordPolicyResponse
newGetAccountPasswordPolicyResponse
  pHttpStatus_
  pPasswordPolicy_ =
    GetAccountPasswordPolicyResponse'
      { httpStatus =
          pHttpStatus_,
        passwordPolicy = pPasswordPolicy_
      }

-- | The response's http status code.
getAccountPasswordPolicyResponse_httpStatus :: Lens.Lens' GetAccountPasswordPolicyResponse Prelude.Int
getAccountPasswordPolicyResponse_httpStatus = Lens.lens (\GetAccountPasswordPolicyResponse' {httpStatus} -> httpStatus) (\s@GetAccountPasswordPolicyResponse' {} a -> s {httpStatus = a} :: GetAccountPasswordPolicyResponse)

-- | A structure that contains details about the account\'s password policy.
getAccountPasswordPolicyResponse_passwordPolicy :: Lens.Lens' GetAccountPasswordPolicyResponse PasswordPolicy
getAccountPasswordPolicyResponse_passwordPolicy = Lens.lens (\GetAccountPasswordPolicyResponse' {passwordPolicy} -> passwordPolicy) (\s@GetAccountPasswordPolicyResponse' {} a -> s {passwordPolicy = a} :: GetAccountPasswordPolicyResponse)

instance
  Prelude.NFData
    GetAccountPasswordPolicyResponse
  where
  rnf GetAccountPasswordPolicyResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf passwordPolicy
