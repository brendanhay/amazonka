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
-- Module      : Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy for the application.
module Network.AWS.ServerlessApplicationRepository.GetApplicationPolicy
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newGetApplicationPolicy' smart constructor.
data GetApplicationPolicy = GetApplicationPolicy'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetApplicationPolicy
newGetApplicationPolicy pApplicationId_ =
  GetApplicationPolicy'
    { applicationId =
        pApplicationId_
    }

-- | The Amazon Resource Name (ARN) of the application.
getApplicationPolicy_applicationId :: Lens.Lens' GetApplicationPolicy Core.Text
getApplicationPolicy_applicationId = Lens.lens (\GetApplicationPolicy' {applicationId} -> applicationId) (\s@GetApplicationPolicy' {} a -> s {applicationId = a} :: GetApplicationPolicy)

instance Core.AWSRequest GetApplicationPolicy where
  type
    AWSResponse GetApplicationPolicy =
      GetApplicationPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationPolicyResponse'
            Core.<$> (x Core..?> "statements" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetApplicationPolicy

instance Core.NFData GetApplicationPolicy

instance Core.ToHeaders GetApplicationPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetApplicationPolicy where
  toPath GetApplicationPolicy' {..} =
    Core.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/policy"
      ]

instance Core.ToQuery GetApplicationPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetApplicationPolicyResponse' smart constructor.
data GetApplicationPolicyResponse = GetApplicationPolicyResponse'
  { -- | An array of policy statements applied to the application.
    statements :: Core.Maybe [ApplicationPolicyStatement],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetApplicationPolicyResponse
newGetApplicationPolicyResponse pHttpStatus_ =
  GetApplicationPolicyResponse'
    { statements =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of policy statements applied to the application.
getApplicationPolicyResponse_statements :: Lens.Lens' GetApplicationPolicyResponse (Core.Maybe [ApplicationPolicyStatement])
getApplicationPolicyResponse_statements = Lens.lens (\GetApplicationPolicyResponse' {statements} -> statements) (\s@GetApplicationPolicyResponse' {} a -> s {statements = a} :: GetApplicationPolicyResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getApplicationPolicyResponse_httpStatus :: Lens.Lens' GetApplicationPolicyResponse Core.Int
getApplicationPolicyResponse_httpStatus = Lens.lens (\GetApplicationPolicyResponse' {httpStatus} -> httpStatus) (\s@GetApplicationPolicyResponse' {} a -> s {httpStatus = a} :: GetApplicationPolicyResponse)

instance Core.NFData GetApplicationPolicyResponse
