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
-- Module      : Amazonka.MediaStore.GetContainerPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the access policy for the specified container. For information
-- about the data that is included in an access policy, see the
-- <https://aws.amazon.com/documentation/iam/ AWS Identity and Access Management User Guide>.
module Amazonka.MediaStore.GetContainerPolicy
  ( -- * Creating a Request
    GetContainerPolicy (..),
    newGetContainerPolicy,

    -- * Request Lenses
    getContainerPolicy_containerName,

    -- * Destructuring the Response
    GetContainerPolicyResponse (..),
    newGetContainerPolicyResponse,

    -- * Response Lenses
    getContainerPolicyResponse_httpStatus,
    getContainerPolicyResponse_policy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetContainerPolicy' smart constructor.
data GetContainerPolicy = GetContainerPolicy'
  { -- | The name of the container.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'getContainerPolicy_containerName' - The name of the container.
newGetContainerPolicy ::
  -- | 'containerName'
  Prelude.Text ->
  GetContainerPolicy
newGetContainerPolicy pContainerName_ =
  GetContainerPolicy'
    { containerName =
        pContainerName_
    }

-- | The name of the container.
getContainerPolicy_containerName :: Lens.Lens' GetContainerPolicy Prelude.Text
getContainerPolicy_containerName = Lens.lens (\GetContainerPolicy' {containerName} -> containerName) (\s@GetContainerPolicy' {} a -> s {containerName = a} :: GetContainerPolicy)

instance Core.AWSRequest GetContainerPolicy where
  type
    AWSResponse GetContainerPolicy =
      GetContainerPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Policy")
      )

instance Prelude.Hashable GetContainerPolicy where
  hashWithSalt _salt GetContainerPolicy' {..} =
    _salt `Prelude.hashWithSalt` containerName

instance Prelude.NFData GetContainerPolicy where
  rnf GetContainerPolicy' {..} =
    Prelude.rnf containerName

instance Data.ToHeaders GetContainerPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.GetContainerPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetContainerPolicy where
  toJSON GetContainerPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Data..= containerName)
          ]
      )

instance Data.ToPath GetContainerPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetContainerPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetContainerPolicyResponse' smart constructor.
data GetContainerPolicyResponse = GetContainerPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The contents of the access policy.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetContainerPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getContainerPolicyResponse_httpStatus' - The response's http status code.
--
-- 'policy', 'getContainerPolicyResponse_policy' - The contents of the access policy.
newGetContainerPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policy'
  Prelude.Text ->
  GetContainerPolicyResponse
newGetContainerPolicyResponse pHttpStatus_ pPolicy_ =
  GetContainerPolicyResponse'
    { httpStatus =
        pHttpStatus_,
      policy = pPolicy_
    }

-- | The response's http status code.
getContainerPolicyResponse_httpStatus :: Lens.Lens' GetContainerPolicyResponse Prelude.Int
getContainerPolicyResponse_httpStatus = Lens.lens (\GetContainerPolicyResponse' {httpStatus} -> httpStatus) (\s@GetContainerPolicyResponse' {} a -> s {httpStatus = a} :: GetContainerPolicyResponse)

-- | The contents of the access policy.
getContainerPolicyResponse_policy :: Lens.Lens' GetContainerPolicyResponse Prelude.Text
getContainerPolicyResponse_policy = Lens.lens (\GetContainerPolicyResponse' {policy} -> policy) (\s@GetContainerPolicyResponse' {} a -> s {policy = a} :: GetContainerPolicyResponse)

instance Prelude.NFData GetContainerPolicyResponse where
  rnf GetContainerPolicyResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf policy
