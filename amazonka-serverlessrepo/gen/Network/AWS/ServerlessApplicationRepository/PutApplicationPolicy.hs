{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newPutApplicationPolicy' smart constructor.
data PutApplicationPolicy = PutApplicationPolicy'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text,
    -- | An array of policy statements applied to the application.
    statements :: [ApplicationPolicyStatement]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  PutApplicationPolicy
newPutApplicationPolicy pApplicationId_ =
  PutApplicationPolicy'
    { applicationId =
        pApplicationId_,
      statements = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the application.
putApplicationPolicy_applicationId :: Lens.Lens' PutApplicationPolicy Prelude.Text
putApplicationPolicy_applicationId = Lens.lens (\PutApplicationPolicy' {applicationId} -> applicationId) (\s@PutApplicationPolicy' {} a -> s {applicationId = a} :: PutApplicationPolicy)

-- | An array of policy statements applied to the application.
putApplicationPolicy_statements :: Lens.Lens' PutApplicationPolicy [ApplicationPolicyStatement]
putApplicationPolicy_statements = Lens.lens (\PutApplicationPolicy' {statements} -> statements) (\s@PutApplicationPolicy' {} a -> s {statements = a} :: PutApplicationPolicy) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest PutApplicationPolicy where
  type
    Rs PutApplicationPolicy =
      PutApplicationPolicyResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutApplicationPolicyResponse'
            Prelude.<$> ( x Prelude..?> "statements"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutApplicationPolicy

instance Prelude.NFData PutApplicationPolicy

instance Prelude.ToHeaders PutApplicationPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutApplicationPolicy where
  toJSON PutApplicationPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("statements" Prelude..= statements)]
      )

instance Prelude.ToPath PutApplicationPolicy where
  toPath PutApplicationPolicy' {..} =
    Prelude.mconcat
      [ "/applications/",
        Prelude.toBS applicationId,
        "/policy"
      ]

instance Prelude.ToQuery PutApplicationPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutApplicationPolicyResponse' smart constructor.
data PutApplicationPolicyResponse = PutApplicationPolicyResponse'
  { -- | An array of policy statements applied to the application.
    statements :: Prelude.Maybe [ApplicationPolicyStatement],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  PutApplicationPolicyResponse
newPutApplicationPolicyResponse pHttpStatus_ =
  PutApplicationPolicyResponse'
    { statements =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of policy statements applied to the application.
putApplicationPolicyResponse_statements :: Lens.Lens' PutApplicationPolicyResponse (Prelude.Maybe [ApplicationPolicyStatement])
putApplicationPolicyResponse_statements = Lens.lens (\PutApplicationPolicyResponse' {statements} -> statements) (\s@PutApplicationPolicyResponse' {} a -> s {statements = a} :: PutApplicationPolicyResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
putApplicationPolicyResponse_httpStatus :: Lens.Lens' PutApplicationPolicyResponse Prelude.Int
putApplicationPolicyResponse_httpStatus = Lens.lens (\PutApplicationPolicyResponse' {httpStatus} -> httpStatus) (\s@PutApplicationPolicyResponse' {} a -> s {httpStatus = a} :: PutApplicationPolicyResponse)

instance Prelude.NFData PutApplicationPolicyResponse
