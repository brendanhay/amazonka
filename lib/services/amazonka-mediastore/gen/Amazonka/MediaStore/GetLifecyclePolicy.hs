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
-- Module      : Amazonka.MediaStore.GetLifecyclePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the object lifecycle policy that is assigned to a container.
module Amazonka.MediaStore.GetLifecyclePolicy
  ( -- * Creating a Request
    GetLifecyclePolicy (..),
    newGetLifecyclePolicy,

    -- * Request Lenses
    getLifecyclePolicy_containerName,

    -- * Destructuring the Response
    GetLifecyclePolicyResponse (..),
    newGetLifecyclePolicyResponse,

    -- * Response Lenses
    getLifecyclePolicyResponse_httpStatus,
    getLifecyclePolicyResponse_lifecyclePolicy,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { -- | The name of the container that the object lifecycle policy is assigned
    -- to.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'getLifecyclePolicy_containerName' - The name of the container that the object lifecycle policy is assigned
-- to.
newGetLifecyclePolicy ::
  -- | 'containerName'
  Prelude.Text ->
  GetLifecyclePolicy
newGetLifecyclePolicy pContainerName_ =
  GetLifecyclePolicy'
    { containerName =
        pContainerName_
    }

-- | The name of the container that the object lifecycle policy is assigned
-- to.
getLifecyclePolicy_containerName :: Lens.Lens' GetLifecyclePolicy Prelude.Text
getLifecyclePolicy_containerName = Lens.lens (\GetLifecyclePolicy' {containerName} -> containerName) (\s@GetLifecyclePolicy' {} a -> s {containerName = a} :: GetLifecyclePolicy)

instance Core.AWSRequest GetLifecyclePolicy where
  type
    AWSResponse GetLifecyclePolicy =
      GetLifecyclePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "LifecyclePolicy")
      )

instance Prelude.Hashable GetLifecyclePolicy where
  hashWithSalt _salt GetLifecyclePolicy' {..} =
    _salt `Prelude.hashWithSalt` containerName

instance Prelude.NFData GetLifecyclePolicy where
  rnf GetLifecyclePolicy' {..} =
    Prelude.rnf containerName

instance Data.ToHeaders GetLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.GetLifecyclePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetLifecyclePolicy where
  toJSON GetLifecyclePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Data..= containerName)
          ]
      )

instance Data.ToPath GetLifecyclePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The object lifecycle policy that is assigned to the container.
    lifecyclePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getLifecyclePolicyResponse_httpStatus' - The response's http status code.
--
-- 'lifecyclePolicy', 'getLifecyclePolicyResponse_lifecyclePolicy' - The object lifecycle policy that is assigned to the container.
newGetLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'lifecyclePolicy'
  Prelude.Text ->
  GetLifecyclePolicyResponse
newGetLifecyclePolicyResponse
  pHttpStatus_
  pLifecyclePolicy_ =
    GetLifecyclePolicyResponse'
      { httpStatus =
          pHttpStatus_,
        lifecyclePolicy = pLifecyclePolicy_
      }

-- | The response's http status code.
getLifecyclePolicyResponse_httpStatus :: Lens.Lens' GetLifecyclePolicyResponse Prelude.Int
getLifecyclePolicyResponse_httpStatus = Lens.lens (\GetLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@GetLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: GetLifecyclePolicyResponse)

-- | The object lifecycle policy that is assigned to the container.
getLifecyclePolicyResponse_lifecyclePolicy :: Lens.Lens' GetLifecyclePolicyResponse Prelude.Text
getLifecyclePolicyResponse_lifecyclePolicy = Lens.lens (\GetLifecyclePolicyResponse' {lifecyclePolicy} -> lifecyclePolicy) (\s@GetLifecyclePolicyResponse' {} a -> s {lifecyclePolicy = a} :: GetLifecyclePolicyResponse)

instance Prelude.NFData GetLifecyclePolicyResponse where
  rnf GetLifecyclePolicyResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf lifecyclePolicy
