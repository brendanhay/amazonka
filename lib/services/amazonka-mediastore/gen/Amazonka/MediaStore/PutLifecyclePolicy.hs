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
-- Module      : Amazonka.MediaStore.PutLifecyclePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.MediaStore.PutLifecyclePolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaStore.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutLifecyclePolicy' smart constructor.
data PutLifecyclePolicy = PutLifecyclePolicy'
  { -- | The name of the container that you want to assign the object lifecycle
    -- policy to.
    containerName :: Prelude.Text,
    -- | The object lifecycle policy to apply to the container.
    lifecyclePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'lifecyclePolicy'
  Prelude.Text ->
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
putLifecyclePolicy_containerName :: Lens.Lens' PutLifecyclePolicy Prelude.Text
putLifecyclePolicy_containerName = Lens.lens (\PutLifecyclePolicy' {containerName} -> containerName) (\s@PutLifecyclePolicy' {} a -> s {containerName = a} :: PutLifecyclePolicy)

-- | The object lifecycle policy to apply to the container.
putLifecyclePolicy_lifecyclePolicy :: Lens.Lens' PutLifecyclePolicy Prelude.Text
putLifecyclePolicy_lifecyclePolicy = Lens.lens (\PutLifecyclePolicy' {lifecyclePolicy} -> lifecyclePolicy) (\s@PutLifecyclePolicy' {} a -> s {lifecyclePolicy = a} :: PutLifecyclePolicy)

instance Core.AWSRequest PutLifecyclePolicy where
  type
    AWSResponse PutLifecyclePolicy =
      PutLifecyclePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutLifecyclePolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutLifecyclePolicy where
  hashWithSalt _salt PutLifecyclePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` containerName
      `Prelude.hashWithSalt` lifecyclePolicy

instance Prelude.NFData PutLifecyclePolicy where
  rnf PutLifecyclePolicy' {..} =
    Prelude.rnf containerName
      `Prelude.seq` Prelude.rnf lifecyclePolicy

instance Data.ToHeaders PutLifecyclePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MediaStore_20170901.PutLifecyclePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutLifecyclePolicy where
  toJSON PutLifecyclePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Data..= containerName),
            Prelude.Just
              ("LifecyclePolicy" Data..= lifecyclePolicy)
          ]
      )

instance Data.ToPath PutLifecyclePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutLifecyclePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutLifecyclePolicyResponse' smart constructor.
data PutLifecyclePolicyResponse = PutLifecyclePolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutLifecyclePolicyResponse
newPutLifecyclePolicyResponse pHttpStatus_ =
  PutLifecyclePolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putLifecyclePolicyResponse_httpStatus :: Lens.Lens' PutLifecyclePolicyResponse Prelude.Int
putLifecyclePolicyResponse_httpStatus = Lens.lens (\PutLifecyclePolicyResponse' {httpStatus} -> httpStatus) (\s@PutLifecyclePolicyResponse' {} a -> s {httpStatus = a} :: PutLifecyclePolicyResponse)

instance Prelude.NFData PutLifecyclePolicyResponse where
  rnf PutLifecyclePolicyResponse' {..} =
    Prelude.rnf httpStatus
