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
-- Module      : Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the permission policy for an application. For the list of actions
-- supported for this operation, see
-- <https://docs.aws.amazon.com/serverlessrepo/latest/devguide/access-control-resource-based.html#application-permissions Application Permissions>
-- .
module Network.AWS.ServerlessApplicationRepository.PutApplicationPolicy
  ( -- * Creating a Request
    PutApplicationPolicy (..),
    newPutApplicationPolicy,

    -- * Request Lenses
    putApplicationPolicy_applicationId,
    putApplicationPolicy_statements,

    -- * Destructuring the Response
    PutApplicationPolicyResponse (..),
    newPutApplicationPolicyResponse,

    -- * Response Lenses
    putApplicationPolicyResponse_statements,
    putApplicationPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newPutApplicationPolicy' smart constructor.
data PutApplicationPolicy = PutApplicationPolicy'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text,
    -- | An array of policy statements applied to the application.
    statements :: [ApplicationPolicyStatement]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutApplicationPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'putApplicationPolicy_applicationId' - The Amazon Resource Name (ARN) of the application.
--
-- 'statements', 'putApplicationPolicy_statements' - An array of policy statements applied to the application.
newPutApplicationPolicy ::
  -- | 'applicationId'
  Core.Text ->
  PutApplicationPolicy
newPutApplicationPolicy pApplicationId_ =
  PutApplicationPolicy'
    { applicationId =
        pApplicationId_,
      statements = Core.mempty
    }

-- | The Amazon Resource Name (ARN) of the application.
putApplicationPolicy_applicationId :: Lens.Lens' PutApplicationPolicy Core.Text
putApplicationPolicy_applicationId = Lens.lens (\PutApplicationPolicy' {applicationId} -> applicationId) (\s@PutApplicationPolicy' {} a -> s {applicationId = a} :: PutApplicationPolicy)

-- | An array of policy statements applied to the application.
putApplicationPolicy_statements :: Lens.Lens' PutApplicationPolicy [ApplicationPolicyStatement]
putApplicationPolicy_statements = Lens.lens (\PutApplicationPolicy' {statements} -> statements) (\s@PutApplicationPolicy' {} a -> s {statements = a} :: PutApplicationPolicy) Core.. Lens._Coerce

instance Core.AWSRequest PutApplicationPolicy where
  type
    AWSResponse PutApplicationPolicy =
      PutApplicationPolicyResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutApplicationPolicyResponse'
            Core.<$> (x Core..?> "statements" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutApplicationPolicy

instance Core.NFData PutApplicationPolicy

instance Core.ToHeaders PutApplicationPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutApplicationPolicy where
  toJSON PutApplicationPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("statements" Core..= statements)]
      )

instance Core.ToPath PutApplicationPolicy where
  toPath PutApplicationPolicy' {..} =
    Core.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/policy"
      ]

instance Core.ToQuery PutApplicationPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutApplicationPolicyResponse' smart constructor.
data PutApplicationPolicyResponse = PutApplicationPolicyResponse'
  { -- | An array of policy statements applied to the application.
    statements :: Core.Maybe [ApplicationPolicyStatement],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutApplicationPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statements', 'putApplicationPolicyResponse_statements' - An array of policy statements applied to the application.
--
-- 'httpStatus', 'putApplicationPolicyResponse_httpStatus' - The response's http status code.
newPutApplicationPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutApplicationPolicyResponse
newPutApplicationPolicyResponse pHttpStatus_ =
  PutApplicationPolicyResponse'
    { statements =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of policy statements applied to the application.
putApplicationPolicyResponse_statements :: Lens.Lens' PutApplicationPolicyResponse (Core.Maybe [ApplicationPolicyStatement])
putApplicationPolicyResponse_statements = Lens.lens (\PutApplicationPolicyResponse' {statements} -> statements) (\s@PutApplicationPolicyResponse' {} a -> s {statements = a} :: PutApplicationPolicyResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
putApplicationPolicyResponse_httpStatus :: Lens.Lens' PutApplicationPolicyResponse Core.Int
putApplicationPolicyResponse_httpStatus = Lens.lens (\PutApplicationPolicyResponse' {httpStatus} -> httpStatus) (\s@PutApplicationPolicyResponse' {} a -> s {httpStatus = a} :: PutApplicationPolicyResponse)

instance Core.NFData PutApplicationPolicyResponse
