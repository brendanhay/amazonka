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
-- Module      : Network.AWS.Route53.DeleteTrafficPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a traffic policy.
--
-- When you delete a traffic policy, Route 53 sets a flag on the policy to
-- indicate that it has been deleted. However, Route 53 never fully deletes
-- the traffic policy. Note the following:
--
-- -   Deleted traffic policies aren\'t listed if you run
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListTrafficPolicies.html ListTrafficPolicies>.
--
-- -   There\'s no way to get a list of deleted policies.
--
-- -   If you retain the ID of the policy, you can get information about
--     the policy, including the traffic policy document, by running
--     <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetTrafficPolicy.html GetTrafficPolicy>.
module Network.AWS.Route53.DeleteTrafficPolicy
  ( -- * Creating a Request
    DeleteTrafficPolicy (..),
    newDeleteTrafficPolicy,

    -- * Request Lenses
    deleteTrafficPolicy_id,
    deleteTrafficPolicy_version,

    -- * Destructuring the Response
    DeleteTrafficPolicyResponse (..),
    newDeleteTrafficPolicyResponse,

    -- * Response Lenses
    deleteTrafficPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A request to delete a specified traffic policy version.
--
-- /See:/ 'newDeleteTrafficPolicy' smart constructor.
data DeleteTrafficPolicy = DeleteTrafficPolicy'
  { -- | The ID of the traffic policy that you want to delete.
    id :: Prelude.Text,
    -- | The version number of the traffic policy that you want to delete.
    version :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrafficPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteTrafficPolicy_id' - The ID of the traffic policy that you want to delete.
--
-- 'version', 'deleteTrafficPolicy_version' - The version number of the traffic policy that you want to delete.
newDeleteTrafficPolicy ::
  -- | 'id'
  Prelude.Text ->
  -- | 'version'
  Prelude.Natural ->
  DeleteTrafficPolicy
newDeleteTrafficPolicy pId_ pVersion_ =
  DeleteTrafficPolicy'
    { id = pId_,
      version = pVersion_
    }

-- | The ID of the traffic policy that you want to delete.
deleteTrafficPolicy_id :: Lens.Lens' DeleteTrafficPolicy Prelude.Text
deleteTrafficPolicy_id = Lens.lens (\DeleteTrafficPolicy' {id} -> id) (\s@DeleteTrafficPolicy' {} a -> s {id = a} :: DeleteTrafficPolicy)

-- | The version number of the traffic policy that you want to delete.
deleteTrafficPolicy_version :: Lens.Lens' DeleteTrafficPolicy Prelude.Natural
deleteTrafficPolicy_version = Lens.lens (\DeleteTrafficPolicy' {version} -> version) (\s@DeleteTrafficPolicy' {} a -> s {version = a} :: DeleteTrafficPolicy)

instance Prelude.AWSRequest DeleteTrafficPolicy where
  type
    Rs DeleteTrafficPolicy =
      DeleteTrafficPolicyResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTrafficPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTrafficPolicy

instance Prelude.NFData DeleteTrafficPolicy

instance Prelude.ToHeaders DeleteTrafficPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteTrafficPolicy where
  toPath DeleteTrafficPolicy' {..} =
    Prelude.mconcat
      [ "/2013-04-01/trafficpolicy/",
        Prelude.toBS id,
        "/",
        Prelude.toBS version
      ]

instance Prelude.ToQuery DeleteTrafficPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | An empty element.
--
-- /See:/ 'newDeleteTrafficPolicyResponse' smart constructor.
data DeleteTrafficPolicyResponse = DeleteTrafficPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteTrafficPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTrafficPolicyResponse_httpStatus' - The response's http status code.
newDeleteTrafficPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTrafficPolicyResponse
newDeleteTrafficPolicyResponse pHttpStatus_ =
  DeleteTrafficPolicyResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTrafficPolicyResponse_httpStatus :: Lens.Lens' DeleteTrafficPolicyResponse Prelude.Int
deleteTrafficPolicyResponse_httpStatus = Lens.lens (\DeleteTrafficPolicyResponse' {httpStatus} -> httpStatus) (\s@DeleteTrafficPolicyResponse' {} a -> s {httpStatus = a} :: DeleteTrafficPolicyResponse)

instance Prelude.NFData DeleteTrafficPolicyResponse
