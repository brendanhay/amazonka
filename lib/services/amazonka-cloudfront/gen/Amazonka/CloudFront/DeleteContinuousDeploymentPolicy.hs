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
-- Module      : Amazonka.CloudFront.DeleteContinuousDeploymentPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a continuous deployment policy.
--
-- You cannot delete a continuous deployment policy that\'s attached to a
-- primary distribution. First update your distribution to remove the
-- continuous deployment policy, then you can delete the policy.
module Amazonka.CloudFront.DeleteContinuousDeploymentPolicy
  ( -- * Creating a Request
    DeleteContinuousDeploymentPolicy (..),
    newDeleteContinuousDeploymentPolicy,

    -- * Request Lenses
    deleteContinuousDeploymentPolicy_ifMatch,
    deleteContinuousDeploymentPolicy_id,

    -- * Destructuring the Response
    DeleteContinuousDeploymentPolicyResponse (..),
    newDeleteContinuousDeploymentPolicyResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteContinuousDeploymentPolicy' smart constructor.
data DeleteContinuousDeploymentPolicy = DeleteContinuousDeploymentPolicy'
  { -- | The current version (@ETag@ value) of the continuous deployment policy
    -- that you are deleting.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the continuous deployment policy that you are
    -- deleting.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContinuousDeploymentPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteContinuousDeploymentPolicy_ifMatch' - The current version (@ETag@ value) of the continuous deployment policy
-- that you are deleting.
--
-- 'id', 'deleteContinuousDeploymentPolicy_id' - The identifier of the continuous deployment policy that you are
-- deleting.
newDeleteContinuousDeploymentPolicy ::
  -- | 'id'
  Prelude.Text ->
  DeleteContinuousDeploymentPolicy
newDeleteContinuousDeploymentPolicy pId_ =
  DeleteContinuousDeploymentPolicy'
    { ifMatch =
        Prelude.Nothing,
      id = pId_
    }

-- | The current version (@ETag@ value) of the continuous deployment policy
-- that you are deleting.
deleteContinuousDeploymentPolicy_ifMatch :: Lens.Lens' DeleteContinuousDeploymentPolicy (Prelude.Maybe Prelude.Text)
deleteContinuousDeploymentPolicy_ifMatch = Lens.lens (\DeleteContinuousDeploymentPolicy' {ifMatch} -> ifMatch) (\s@DeleteContinuousDeploymentPolicy' {} a -> s {ifMatch = a} :: DeleteContinuousDeploymentPolicy)

-- | The identifier of the continuous deployment policy that you are
-- deleting.
deleteContinuousDeploymentPolicy_id :: Lens.Lens' DeleteContinuousDeploymentPolicy Prelude.Text
deleteContinuousDeploymentPolicy_id = Lens.lens (\DeleteContinuousDeploymentPolicy' {id} -> id) (\s@DeleteContinuousDeploymentPolicy' {} a -> s {id = a} :: DeleteContinuousDeploymentPolicy)

instance
  Core.AWSRequest
    DeleteContinuousDeploymentPolicy
  where
  type
    AWSResponse DeleteContinuousDeploymentPolicy =
      DeleteContinuousDeploymentPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteContinuousDeploymentPolicyResponse'

instance
  Prelude.Hashable
    DeleteContinuousDeploymentPolicy
  where
  hashWithSalt
    _salt
    DeleteContinuousDeploymentPolicy' {..} =
      _salt
        `Prelude.hashWithSalt` ifMatch
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DeleteContinuousDeploymentPolicy
  where
  rnf DeleteContinuousDeploymentPolicy' {..} =
    Prelude.rnf ifMatch `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    DeleteContinuousDeploymentPolicy
  where
  toHeaders DeleteContinuousDeploymentPolicy' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath DeleteContinuousDeploymentPolicy where
  toPath DeleteContinuousDeploymentPolicy' {..} =
    Prelude.mconcat
      [ "/2020-05-31/continuous-deployment-policy/",
        Data.toBS id
      ]

instance
  Data.ToQuery
    DeleteContinuousDeploymentPolicy
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContinuousDeploymentPolicyResponse' smart constructor.
data DeleteContinuousDeploymentPolicyResponse = DeleteContinuousDeploymentPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContinuousDeploymentPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteContinuousDeploymentPolicyResponse ::
  DeleteContinuousDeploymentPolicyResponse
newDeleteContinuousDeploymentPolicyResponse =
  DeleteContinuousDeploymentPolicyResponse'

instance
  Prelude.NFData
    DeleteContinuousDeploymentPolicyResponse
  where
  rnf _ = ()
