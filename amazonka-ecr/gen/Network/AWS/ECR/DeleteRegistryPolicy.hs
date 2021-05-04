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
-- Module      : Network.AWS.ECR.DeleteRegistryPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the registry permissions policy.
module Network.AWS.ECR.DeleteRegistryPolicy
  ( -- * Creating a Request
    DeleteRegistryPolicy (..),
    newDeleteRegistryPolicy,

    -- * Destructuring the Response
    DeleteRegistryPolicyResponse (..),
    newDeleteRegistryPolicyResponse,

    -- * Response Lenses
    deleteRegistryPolicyResponse_registryId,
    deleteRegistryPolicyResponse_policyText,
    deleteRegistryPolicyResponse_httpStatus,
  )
where

import Network.AWS.ECR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteRegistryPolicy' smart constructor.
data DeleteRegistryPolicy = DeleteRegistryPolicy'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRegistryPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRegistryPolicy ::
  DeleteRegistryPolicy
newDeleteRegistryPolicy = DeleteRegistryPolicy'

instance Prelude.AWSRequest DeleteRegistryPolicy where
  type
    Rs DeleteRegistryPolicy =
      DeleteRegistryPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteRegistryPolicyResponse'
            Prelude.<$> (x Prelude..?> "registryId")
            Prelude.<*> (x Prelude..?> "policyText")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRegistryPolicy

instance Prelude.NFData DeleteRegistryPolicy

instance Prelude.ToHeaders DeleteRegistryPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerRegistry_V20150921.DeleteRegistryPolicy" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteRegistryPolicy where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DeleteRegistryPolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRegistryPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRegistryPolicyResponse' smart constructor.
data DeleteRegistryPolicyResponse = DeleteRegistryPolicyResponse'
  { -- | The registry ID associated with the request.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The contents of the registry permissions policy that was deleted.
    policyText :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteRegistryPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'deleteRegistryPolicyResponse_registryId' - The registry ID associated with the request.
--
-- 'policyText', 'deleteRegistryPolicyResponse_policyText' - The contents of the registry permissions policy that was deleted.
--
-- 'httpStatus', 'deleteRegistryPolicyResponse_httpStatus' - The response's http status code.
newDeleteRegistryPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteRegistryPolicyResponse
newDeleteRegistryPolicyResponse pHttpStatus_ =
  DeleteRegistryPolicyResponse'
    { registryId =
        Prelude.Nothing,
      policyText = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The registry ID associated with the request.
deleteRegistryPolicyResponse_registryId :: Lens.Lens' DeleteRegistryPolicyResponse (Prelude.Maybe Prelude.Text)
deleteRegistryPolicyResponse_registryId = Lens.lens (\DeleteRegistryPolicyResponse' {registryId} -> registryId) (\s@DeleteRegistryPolicyResponse' {} a -> s {registryId = a} :: DeleteRegistryPolicyResponse)

-- | The contents of the registry permissions policy that was deleted.
deleteRegistryPolicyResponse_policyText :: Lens.Lens' DeleteRegistryPolicyResponse (Prelude.Maybe Prelude.Text)
deleteRegistryPolicyResponse_policyText = Lens.lens (\DeleteRegistryPolicyResponse' {policyText} -> policyText) (\s@DeleteRegistryPolicyResponse' {} a -> s {policyText = a} :: DeleteRegistryPolicyResponse)

-- | The response's http status code.
deleteRegistryPolicyResponse_httpStatus :: Lens.Lens' DeleteRegistryPolicyResponse Prelude.Int
deleteRegistryPolicyResponse_httpStatus = Lens.lens (\DeleteRegistryPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteRegistryPolicyResponse' {} a -> s {httpStatus = a} :: DeleteRegistryPolicyResponse)

instance Prelude.NFData DeleteRegistryPolicyResponse
