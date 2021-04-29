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
-- Module      : Network.AWS.MediaStore.GetLifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the object lifecycle policy that is assigned to a container.
module Network.AWS.MediaStore.GetLifecyclePolicy
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetLifecyclePolicy' smart constructor.
data GetLifecyclePolicy = GetLifecyclePolicy'
  { -- | The name of the container that the object lifecycle policy is assigned
    -- to.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetLifecyclePolicy where
  type
    Rs GetLifecyclePolicy =
      GetLifecyclePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLifecyclePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "LifecyclePolicy")
      )

instance Prelude.Hashable GetLifecyclePolicy

instance Prelude.NFData GetLifecyclePolicy

instance Prelude.ToHeaders GetLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MediaStore_20170901.GetLifecyclePolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetLifecyclePolicy where
  toJSON GetLifecyclePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Prelude..= containerName)
          ]
      )

instance Prelude.ToPath GetLifecyclePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLifecyclePolicyResponse' smart constructor.
data GetLifecyclePolicyResponse = GetLifecyclePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The object lifecycle policy that is assigned to the container.
    lifecyclePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetLifecyclePolicyResponse
