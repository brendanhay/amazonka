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
-- Module      : Network.AWS.MediaStore.PutContainerPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an access policy for the specified container to restrict the
-- users and clients that can access it. For information about the data
-- that is included in an access policy, see the
-- <https://aws.amazon.com/documentation/iam/ AWS Identity and Access Management User Guide>.
--
-- For this release of the REST API, you can create only one policy for a
-- container. If you enter @PutContainerPolicy@ twice, the second command
-- modifies the existing policy.
module Network.AWS.MediaStore.PutContainerPolicy
  ( -- * Creating a Request
    PutContainerPolicy (..),
    newPutContainerPolicy,

    -- * Request Lenses
    putContainerPolicy_containerName,
    putContainerPolicy_policy,

    -- * Destructuring the Response
    PutContainerPolicyResponse (..),
    newPutContainerPolicyResponse,

    -- * Response Lenses
    putContainerPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaStore.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutContainerPolicy' smart constructor.
data PutContainerPolicy = PutContainerPolicy'
  { -- | The name of the container.
    containerName :: Prelude.Text,
    -- | The contents of the policy, which includes the following:
    --
    -- -   One @Version@ tag
    --
    -- -   One @Statement@ tag that contains the standard tags for the policy.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutContainerPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerName', 'putContainerPolicy_containerName' - The name of the container.
--
-- 'policy', 'putContainerPolicy_policy' - The contents of the policy, which includes the following:
--
-- -   One @Version@ tag
--
-- -   One @Statement@ tag that contains the standard tags for the policy.
newPutContainerPolicy ::
  -- | 'containerName'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutContainerPolicy
newPutContainerPolicy pContainerName_ pPolicy_ =
  PutContainerPolicy'
    { containerName =
        pContainerName_,
      policy = pPolicy_
    }

-- | The name of the container.
putContainerPolicy_containerName :: Lens.Lens' PutContainerPolicy Prelude.Text
putContainerPolicy_containerName = Lens.lens (\PutContainerPolicy' {containerName} -> containerName) (\s@PutContainerPolicy' {} a -> s {containerName = a} :: PutContainerPolicy)

-- | The contents of the policy, which includes the following:
--
-- -   One @Version@ tag
--
-- -   One @Statement@ tag that contains the standard tags for the policy.
putContainerPolicy_policy :: Lens.Lens' PutContainerPolicy Prelude.Text
putContainerPolicy_policy = Lens.lens (\PutContainerPolicy' {policy} -> policy) (\s@PutContainerPolicy' {} a -> s {policy = a} :: PutContainerPolicy)

instance Prelude.AWSRequest PutContainerPolicy where
  type
    Rs PutContainerPolicy =
      PutContainerPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutContainerPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutContainerPolicy

instance Prelude.NFData PutContainerPolicy

instance Prelude.ToHeaders PutContainerPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MediaStore_20170901.PutContainerPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutContainerPolicy where
  toJSON PutContainerPolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ContainerName" Prelude..= containerName),
            Prelude.Just ("Policy" Prelude..= policy)
          ]
      )

instance Prelude.ToPath PutContainerPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutContainerPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutContainerPolicyResponse' smart constructor.
data PutContainerPolicyResponse = PutContainerPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutContainerPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putContainerPolicyResponse_httpStatus' - The response's http status code.
newPutContainerPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutContainerPolicyResponse
newPutContainerPolicyResponse pHttpStatus_ =
  PutContainerPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putContainerPolicyResponse_httpStatus :: Lens.Lens' PutContainerPolicyResponse Prelude.Int
putContainerPolicyResponse_httpStatus = Lens.lens (\PutContainerPolicyResponse' {httpStatus} -> httpStatus) (\s@PutContainerPolicyResponse' {} a -> s {httpStatus = a} :: PutContainerPolicyResponse)

instance Prelude.NFData PutContainerPolicyResponse
