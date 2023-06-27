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
-- Module      : Amazonka.CloudDirectory.AttachPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a policy object to a regular object. An object can have a
-- limited number of attached policies.
module Amazonka.CloudDirectory.AttachPolicy
  ( -- * Creating a Request
    AttachPolicy (..),
    newAttachPolicy,

    -- * Request Lenses
    attachPolicy_directoryArn,
    attachPolicy_policyReference,
    attachPolicy_objectReference,

    -- * Destructuring the Response
    AttachPolicyResponse (..),
    newAttachPolicyResponse,

    -- * Response Lenses
    attachPolicyResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachPolicy' smart constructor.
data AttachPolicy = AttachPolicy'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where both objects reside. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | The reference that is associated with the policy object.
    policyReference :: ObjectReference,
    -- | The reference that identifies the object to which the policy will be
    -- attached.
    objectReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'attachPolicy_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where both objects reside. For more information, see arns.
--
-- 'policyReference', 'attachPolicy_policyReference' - The reference that is associated with the policy object.
--
-- 'objectReference', 'attachPolicy_objectReference' - The reference that identifies the object to which the policy will be
-- attached.
newAttachPolicy ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'policyReference'
  ObjectReference ->
  -- | 'objectReference'
  ObjectReference ->
  AttachPolicy
newAttachPolicy
  pDirectoryArn_
  pPolicyReference_
  pObjectReference_ =
    AttachPolicy'
      { directoryArn = pDirectoryArn_,
        policyReference = pPolicyReference_,
        objectReference = pObjectReference_
      }

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where both objects reside. For more information, see arns.
attachPolicy_directoryArn :: Lens.Lens' AttachPolicy Prelude.Text
attachPolicy_directoryArn = Lens.lens (\AttachPolicy' {directoryArn} -> directoryArn) (\s@AttachPolicy' {} a -> s {directoryArn = a} :: AttachPolicy)

-- | The reference that is associated with the policy object.
attachPolicy_policyReference :: Lens.Lens' AttachPolicy ObjectReference
attachPolicy_policyReference = Lens.lens (\AttachPolicy' {policyReference} -> policyReference) (\s@AttachPolicy' {} a -> s {policyReference = a} :: AttachPolicy)

-- | The reference that identifies the object to which the policy will be
-- attached.
attachPolicy_objectReference :: Lens.Lens' AttachPolicy ObjectReference
attachPolicy_objectReference = Lens.lens (\AttachPolicy' {objectReference} -> objectReference) (\s@AttachPolicy' {} a -> s {objectReference = a} :: AttachPolicy)

instance Core.AWSRequest AttachPolicy where
  type AWSResponse AttachPolicy = AttachPolicyResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AttachPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachPolicy where
  hashWithSalt _salt AttachPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` policyReference
      `Prelude.hashWithSalt` objectReference

instance Prelude.NFData AttachPolicy where
  rnf AttachPolicy' {..} =
    Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf policyReference
      `Prelude.seq` Prelude.rnf objectReference

instance Data.ToHeaders AttachPolicy where
  toHeaders AttachPolicy' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# directoryArn]

instance Data.ToJSON AttachPolicy where
  toJSON AttachPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PolicyReference" Data..= policyReference),
            Prelude.Just
              ("ObjectReference" Data..= objectReference)
          ]
      )

instance Data.ToPath AttachPolicy where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/policy/attach"

instance Data.ToQuery AttachPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachPolicyResponse' smart constructor.
data AttachPolicyResponse = AttachPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'attachPolicyResponse_httpStatus' - The response's http status code.
newAttachPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachPolicyResponse
newAttachPolicyResponse pHttpStatus_ =
  AttachPolicyResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
attachPolicyResponse_httpStatus :: Lens.Lens' AttachPolicyResponse Prelude.Int
attachPolicyResponse_httpStatus = Lens.lens (\AttachPolicyResponse' {httpStatus} -> httpStatus) (\s@AttachPolicyResponse' {} a -> s {httpStatus = a} :: AttachPolicyResponse)

instance Prelude.NFData AttachPolicyResponse where
  rnf AttachPolicyResponse' {..} =
    Prelude.rnf httpStatus
