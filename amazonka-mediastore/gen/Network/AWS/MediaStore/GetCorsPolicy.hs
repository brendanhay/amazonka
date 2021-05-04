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
-- Module      : Network.AWS.MediaStore.GetCorsPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cross-origin resource sharing (CORS) configuration
-- information that is set for the container.
--
-- To use this operation, you must have permission to perform the
-- @MediaStore:GetCorsPolicy@ action. By default, the container owner has
-- this permission and can grant it to others.
module Network.AWS.MediaStore.GetCorsPolicy
  ( -- * Creating a Request
    GetCorsPolicy (..),
    newGetCorsPolicy,

    -- * Request Lenses
    getCorsPolicy_containerName,

    -- * Destructuring the Response
    GetCorsPolicyResponse (..),
    newGetCorsPolicyResponse,

    -- * Response Lenses
    getCorsPolicyResponse_httpStatus,
    getCorsPolicyResponse_corsPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCorsPolicy' smart constructor.
data GetCorsPolicy = GetCorsPolicy'
  { -- | The name of the container that the policy is assigned to.
    containerName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCorsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'getCorsPolicy_containerName' - The name of the container that the policy is assigned to.
newGetCorsPolicy ::
  -- | 'containerName'
  Prelude.Text ->
  GetCorsPolicy
newGetCorsPolicy pContainerName_ =
  GetCorsPolicy' {containerName = pContainerName_}

-- | The name of the container that the policy is assigned to.
getCorsPolicy_containerName :: Lens.Lens' GetCorsPolicy Prelude.Text
getCorsPolicy_containerName = Lens.lens (\GetCorsPolicy' {containerName} -> containerName) (\s@GetCorsPolicy' {} a -> s {containerName = a} :: GetCorsPolicy)

instance Prelude.AWSRequest GetCorsPolicy where
  type Rs GetCorsPolicy = GetCorsPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCorsPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "CorsPolicy")
      )

instance Prelude.Hashable GetCorsPolicy

instance Prelude.NFData GetCorsPolicy

instance Prelude.ToHeaders GetCorsPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MediaStore_20170901.GetCorsPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetCorsPolicy where
  toJSON GetCorsPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Prelude..= containerName)
          ]
      )

instance Prelude.ToPath GetCorsPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCorsPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCorsPolicyResponse' smart constructor.
data GetCorsPolicyResponse = GetCorsPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The CORS policy assigned to the container.
    corsPolicy :: Prelude.NonEmpty CorsRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetCorsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getCorsPolicyResponse_httpStatus' - The response's http status code.
--
-- 'corsPolicy', 'getCorsPolicyResponse_corsPolicy' - The CORS policy assigned to the container.
newGetCorsPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'corsPolicy'
  Prelude.NonEmpty CorsRule ->
  GetCorsPolicyResponse
newGetCorsPolicyResponse pHttpStatus_ pCorsPolicy_ =
  GetCorsPolicyResponse'
    { httpStatus = pHttpStatus_,
      corsPolicy = Prelude._Coerce Lens.# pCorsPolicy_
    }

-- | The response's http status code.
getCorsPolicyResponse_httpStatus :: Lens.Lens' GetCorsPolicyResponse Prelude.Int
getCorsPolicyResponse_httpStatus = Lens.lens (\GetCorsPolicyResponse' {httpStatus} -> httpStatus) (\s@GetCorsPolicyResponse' {} a -> s {httpStatus = a} :: GetCorsPolicyResponse)

-- | The CORS policy assigned to the container.
getCorsPolicyResponse_corsPolicy :: Lens.Lens' GetCorsPolicyResponse (Prelude.NonEmpty CorsRule)
getCorsPolicyResponse_corsPolicy = Lens.lens (\GetCorsPolicyResponse' {corsPolicy} -> corsPolicy) (\s@GetCorsPolicyResponse' {} a -> s {corsPolicy = a} :: GetCorsPolicyResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData GetCorsPolicyResponse
