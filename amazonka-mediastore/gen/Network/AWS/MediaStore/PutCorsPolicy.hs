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
-- Module      : Network.AWS.MediaStore.PutCorsPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the cross-origin resource sharing (CORS) configuration on a
-- container so that the container can service cross-origin requests. For
-- example, you might want to enable a request whose origin is
-- http:\/\/www.example.com to access your AWS Elemental MediaStore
-- container at my.example.container.com by using the browser\'s
-- XMLHttpRequest capability.
--
-- To enable CORS on a container, you attach a CORS policy to the
-- container. In the CORS policy, you configure rules that identify origins
-- and the HTTP methods that can be executed on your container. The policy
-- can contain up to 398,000 characters. You can add up to 100 rules to a
-- CORS policy. If more than one rule applies, the service uses the first
-- applicable rule listed.
--
-- To learn more about CORS, see
-- <https://docs.aws.amazon.com/mediastore/latest/ug/cors-policy.html Cross-Origin Resource Sharing (CORS) in AWS Elemental MediaStore>.
module Network.AWS.MediaStore.PutCorsPolicy
  ( -- * Creating a Request
    PutCorsPolicy (..),
    newPutCorsPolicy,

    -- * Request Lenses
    putCorsPolicy_containerName,
    putCorsPolicy_corsPolicy,

    -- * Destructuring the Response
    PutCorsPolicyResponse (..),
    newPutCorsPolicyResponse,

    -- * Response Lenses
    putCorsPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutCorsPolicy' smart constructor.
data PutCorsPolicy = PutCorsPolicy'
  { -- | The name of the container that you want to assign the CORS policy to.
    containerName :: Core.Text,
    -- | The CORS policy to apply to the container.
    corsPolicy :: Core.NonEmpty CorsRule
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutCorsPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'putCorsPolicy_containerName' - The name of the container that you want to assign the CORS policy to.
--
-- 'corsPolicy', 'putCorsPolicy_corsPolicy' - The CORS policy to apply to the container.
newPutCorsPolicy ::
  -- | 'containerName'
  Core.Text ->
  -- | 'corsPolicy'
  Core.NonEmpty CorsRule ->
  PutCorsPolicy
newPutCorsPolicy pContainerName_ pCorsPolicy_ =
  PutCorsPolicy'
    { containerName = pContainerName_,
      corsPolicy = Lens._Coerce Lens.# pCorsPolicy_
    }

-- | The name of the container that you want to assign the CORS policy to.
putCorsPolicy_containerName :: Lens.Lens' PutCorsPolicy Core.Text
putCorsPolicy_containerName = Lens.lens (\PutCorsPolicy' {containerName} -> containerName) (\s@PutCorsPolicy' {} a -> s {containerName = a} :: PutCorsPolicy)

-- | The CORS policy to apply to the container.
putCorsPolicy_corsPolicy :: Lens.Lens' PutCorsPolicy (Core.NonEmpty CorsRule)
putCorsPolicy_corsPolicy = Lens.lens (\PutCorsPolicy' {corsPolicy} -> corsPolicy) (\s@PutCorsPolicy' {} a -> s {corsPolicy = a} :: PutCorsPolicy) Core.. Lens._Coerce

instance Core.AWSRequest PutCorsPolicy where
  type
    AWSResponse PutCorsPolicy =
      PutCorsPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutCorsPolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutCorsPolicy

instance Core.NFData PutCorsPolicy

instance Core.ToHeaders PutCorsPolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.PutCorsPolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutCorsPolicy where
  toJSON PutCorsPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContainerName" Core..= containerName),
            Core.Just ("CorsPolicy" Core..= corsPolicy)
          ]
      )

instance Core.ToPath PutCorsPolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutCorsPolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutCorsPolicyResponse' smart constructor.
data PutCorsPolicyResponse = PutCorsPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutCorsPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putCorsPolicyResponse_httpStatus' - The response's http status code.
newPutCorsPolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutCorsPolicyResponse
newPutCorsPolicyResponse pHttpStatus_ =
  PutCorsPolicyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
putCorsPolicyResponse_httpStatus :: Lens.Lens' PutCorsPolicyResponse Core.Int
putCorsPolicyResponse_httpStatus = Lens.lens (\PutCorsPolicyResponse' {httpStatus} -> httpStatus) (\s@PutCorsPolicyResponse' {} a -> s {httpStatus = a} :: PutCorsPolicyResponse)

instance Core.NFData PutCorsPolicyResponse
