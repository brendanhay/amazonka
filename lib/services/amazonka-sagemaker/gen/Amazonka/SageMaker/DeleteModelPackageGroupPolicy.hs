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
-- Module      : Amazonka.SageMaker.DeleteModelPackageGroupPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a model group resource policy.
module Amazonka.SageMaker.DeleteModelPackageGroupPolicy
  ( -- * Creating a Request
    DeleteModelPackageGroupPolicy (..),
    newDeleteModelPackageGroupPolicy,

    -- * Request Lenses
    deleteModelPackageGroupPolicy_modelPackageGroupName,

    -- * Destructuring the Response
    DeleteModelPackageGroupPolicyResponse (..),
    newDeleteModelPackageGroupPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDeleteModelPackageGroupPolicy' smart constructor.
data DeleteModelPackageGroupPolicy = DeleteModelPackageGroupPolicy'
  { -- | The name of the model group for which to delete the policy.
    modelPackageGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelPackageGroupPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelPackageGroupName', 'deleteModelPackageGroupPolicy_modelPackageGroupName' - The name of the model group for which to delete the policy.
newDeleteModelPackageGroupPolicy ::
  -- | 'modelPackageGroupName'
  Prelude.Text ->
  DeleteModelPackageGroupPolicy
newDeleteModelPackageGroupPolicy
  pModelPackageGroupName_ =
    DeleteModelPackageGroupPolicy'
      { modelPackageGroupName =
          pModelPackageGroupName_
      }

-- | The name of the model group for which to delete the policy.
deleteModelPackageGroupPolicy_modelPackageGroupName :: Lens.Lens' DeleteModelPackageGroupPolicy Prelude.Text
deleteModelPackageGroupPolicy_modelPackageGroupName = Lens.lens (\DeleteModelPackageGroupPolicy' {modelPackageGroupName} -> modelPackageGroupName) (\s@DeleteModelPackageGroupPolicy' {} a -> s {modelPackageGroupName = a} :: DeleteModelPackageGroupPolicy)

instance
  Core.AWSRequest
    DeleteModelPackageGroupPolicy
  where
  type
    AWSResponse DeleteModelPackageGroupPolicy =
      DeleteModelPackageGroupPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeleteModelPackageGroupPolicyResponse'

instance
  Prelude.Hashable
    DeleteModelPackageGroupPolicy
  where
  hashWithSalt _salt DeleteModelPackageGroupPolicy' {..} =
    _salt `Prelude.hashWithSalt` modelPackageGroupName

instance Prelude.NFData DeleteModelPackageGroupPolicy where
  rnf DeleteModelPackageGroupPolicy' {..} =
    Prelude.rnf modelPackageGroupName

instance Core.ToHeaders DeleteModelPackageGroupPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DeleteModelPackageGroupPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteModelPackageGroupPolicy where
  toJSON DeleteModelPackageGroupPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ModelPackageGroupName"
                  Core..= modelPackageGroupName
              )
          ]
      )

instance Core.ToPath DeleteModelPackageGroupPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteModelPackageGroupPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteModelPackageGroupPolicyResponse' smart constructor.
data DeleteModelPackageGroupPolicyResponse = DeleteModelPackageGroupPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteModelPackageGroupPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteModelPackageGroupPolicyResponse ::
  DeleteModelPackageGroupPolicyResponse
newDeleteModelPackageGroupPolicyResponse =
  DeleteModelPackageGroupPolicyResponse'

instance
  Prelude.NFData
    DeleteModelPackageGroupPolicyResponse
  where
  rnf _ = ()
