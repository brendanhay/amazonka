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
-- Module      : Amazonka.ServerlessApplicationRepository.GetApplicationPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy for the application.
module Amazonka.ServerlessApplicationRepository.GetApplicationPolicy
  ( -- * Creating a Request
    GetApplicationPolicy (..),
    newGetApplicationPolicy,

    -- * Request Lenses
    getApplicationPolicy_applicationId,

    -- * Destructuring the Response
    GetApplicationPolicyResponse (..),
    newGetApplicationPolicyResponse,

    -- * Response Lenses
    getApplicationPolicyResponse_statements,
    getApplicationPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServerlessApplicationRepository.Types

-- | /See:/ 'newGetApplicationPolicy' smart constructor.
data GetApplicationPolicy = GetApplicationPolicy'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getApplicationPolicy_applicationId' - The Amazon Resource Name (ARN) of the application.
newGetApplicationPolicy ::
  -- | 'applicationId'
  Prelude.Text ->
  GetApplicationPolicy
newGetApplicationPolicy pApplicationId_ =
  GetApplicationPolicy'
    { applicationId =
        pApplicationId_
    }

-- | The Amazon Resource Name (ARN) of the application.
getApplicationPolicy_applicationId :: Lens.Lens' GetApplicationPolicy Prelude.Text
getApplicationPolicy_applicationId = Lens.lens (\GetApplicationPolicy' {applicationId} -> applicationId) (\s@GetApplicationPolicy' {} a -> s {applicationId = a} :: GetApplicationPolicy)

instance Core.AWSRequest GetApplicationPolicy where
  type
    AWSResponse GetApplicationPolicy =
      GetApplicationPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationPolicyResponse'
            Prelude.<$> (x Core..?> "statements" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApplicationPolicy where
  hashWithSalt _salt GetApplicationPolicy' {..} =
    _salt `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetApplicationPolicy where
  rnf GetApplicationPolicy' {..} =
    Prelude.rnf applicationId

instance Core.ToHeaders GetApplicationPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetApplicationPolicy where
  toPath GetApplicationPolicy' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/policy"
      ]

instance Core.ToQuery GetApplicationPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetApplicationPolicyResponse' smart constructor.
data GetApplicationPolicyResponse = GetApplicationPolicyResponse'
  { -- | An array of policy statements applied to the application.
    statements :: Prelude.Maybe [ApplicationPolicyStatement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statements', 'getApplicationPolicyResponse_statements' - An array of policy statements applied to the application.
--
-- 'httpStatus', 'getApplicationPolicyResponse_httpStatus' - The response's http status code.
newGetApplicationPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApplicationPolicyResponse
newGetApplicationPolicyResponse pHttpStatus_ =
  GetApplicationPolicyResponse'
    { statements =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of policy statements applied to the application.
getApplicationPolicyResponse_statements :: Lens.Lens' GetApplicationPolicyResponse (Prelude.Maybe [ApplicationPolicyStatement])
getApplicationPolicyResponse_statements = Lens.lens (\GetApplicationPolicyResponse' {statements} -> statements) (\s@GetApplicationPolicyResponse' {} a -> s {statements = a} :: GetApplicationPolicyResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getApplicationPolicyResponse_httpStatus :: Lens.Lens' GetApplicationPolicyResponse Prelude.Int
getApplicationPolicyResponse_httpStatus = Lens.lens (\GetApplicationPolicyResponse' {httpStatus} -> httpStatus) (\s@GetApplicationPolicyResponse' {} a -> s {httpStatus = a} :: GetApplicationPolicyResponse)

instance Prelude.NFData GetApplicationPolicyResponse where
  rnf GetApplicationPolicyResponse' {..} =
    Prelude.rnf statements
      `Prelude.seq` Prelude.rnf httpStatus
