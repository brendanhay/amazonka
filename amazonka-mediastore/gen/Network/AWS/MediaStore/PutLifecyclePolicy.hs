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
-- Module      : Network.AWS.MediaStore.PutLifecyclePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Writes an object lifecycle policy to a container. If the container
-- already has an object lifecycle policy, the service replaces the
-- existing policy with the new policy. It takes up to 20 minutes for the
-- change to take effect.
--
-- For information about how to construct an object lifecycle policy, see
-- <https://docs.aws.amazon.com/mediastore/latest/ug/policies-object-lifecycle-components.html Components of an Object Lifecycle Policy>.
module Network.AWS.MediaStore.PutLifecyclePolicy
  ( -- * Creating a Request
    PutLifecyclePolicy (..),
    newPutLifecyclePolicy,

    -- * Request Lenses
    putLifecyclePolicy_containerName,
    putLifecyclePolicy_lifecyclePolicy,

    -- * Destructuring the Response
    PutLifecyclePolicyResponse (..),
    newPutLifecyclePolicyResponse,

    -- * Response Lenses
    putLifecyclePolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { -- | The name of the container that you want to assign the object lifecycle
    -- policy to.
    containerName :: Core.Text,
    -- | The object lifecycle policy to apply to the container.
    lifecyclePolicy :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutLifecyclePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'putLifecyclePolicy_containerName' - The name of the container that you want to assign the object lifecycle
-- policy to.
--
-- 'lifecyclePolicy', 'putLifecyclePolicy_lifecyclePolicy' - The object lifecycle policy to apply to the container.
newPutLifecyclePolicy ::
  -- | 'containerName'
  Core.Text ->
  -- | 'lifecyclePolicy'
  Core.Text ->
  PutLifecyclePolicy
newPutLifecyclePolicy
  pContainerName_
  pLifecyclePolicy_ =
    PutLifecyclePolicy'
      { containerName =
          pContainerName_,
        lifecyclePolicy = pLifecyclePolicy_
      }

-- | The name of the container that you want to assign the object lifecycle
-- policy to.
putLifecyclePolicy_containerName :: Lens.Lens' PutLifecyclePolicy Core.Text
putLifecyclePolicy_containerName = Lens.lens (\PutLifecyclePolicy' {containerName} -> containerName) (\s@PutLifecyclePolicy' {} a -> s {containerName = a} :: PutLifecyclePolicy)

-- | The object lifecycle policy to apply to the container.
putLifecyclePolicy_lifecyclePolicy :: Lens.Lens' PutLifecyclePolicy Core.Text
putLifecyclePolicy_lifecyclePolicy = Lens.lens (\PutLifecyclePolicy' {lifecyclePolicy} -> lifecyclePolicy) (\s@PutLifecyclePolicy' {} a -> s {lifecyclePolicy = a} :: PutLifecyclePolicy)

instance Core.AWSRequest PutLifecyclePolicy where
  type
    AWSResponse PutLifecyclePolicy =
      PutLifecyclePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutLifecyclePolicyResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutLifecyclePolicy

instance Core.NFData PutLifecyclePolicy

instance Core.ToHeaders PutLifecyclePolicy where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MediaStore_20170901.PutLifecyclePolicy" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutLifecyclePolicy where
  toJSON PutLifecyclePolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ContainerName" Core..= containerName),
            Core.Just
              ("LifecyclePolicy" Core..= lifecyclePolicy)
          ]
      )

instance Core.ToPath PutLifecyclePolicy where
  toPath = Core.const "/"

instance Core.ToQuery PutLifecyclePolicy where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newPutLifecyclePolicyResponse' smart constructor.
data PutLifecyclePolicyResponse = PutLifecyclePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutLifecyclePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putLifecyclePolicyResponse_httpStatus' - The response's http status code.
newPutLifecyclePolicyResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutLifecyclePolicyResponse
newPutLifecyclePolicyResponse pHttpStatus_ =
  PutLifecyclePolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putLifecyclePolicyResponse_httpStatus :: Lens.Lens' PutLifecyclePolicyResponse Core.Int
putLifecyclePolicyResponse_httpStatus = Lens.lens (\PutLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@PutLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: PutLifecyclePolicyResponse)

instance Core.NFData PutLifecyclePolicyResponse
