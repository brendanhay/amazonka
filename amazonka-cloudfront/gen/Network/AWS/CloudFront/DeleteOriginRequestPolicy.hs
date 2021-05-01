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
-- Module      : Network.AWS.CloudFront.DeleteOriginRequestPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an origin request policy.
--
-- You cannot delete an origin request policy if it’s attached to any cache
-- behaviors. First update your distributions to remove the origin request
-- policy from all cache behaviors, then delete the origin request policy.
--
-- To delete an origin request policy, you must provide the policy’s
-- identifier and version. To get the identifier, you can use
-- @ListOriginRequestPolicies@ or @GetOriginRequestPolicy@.
module Network.AWS.CloudFront.DeleteOriginRequestPolicy
  ( -- * Creating a Request
    DeleteOriginRequestPolicy (..),
    newDeleteOriginRequestPolicy,

    -- * Request Lenses
    deleteOriginRequestPolicy_ifMatch,
    deleteOriginRequestPolicy_id,

    -- * Destructuring the Response
    DeleteOriginRequestPolicyResponse (..),
    newDeleteOriginRequestPolicyResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteOriginRequestPolicy' smart constructor.
data DeleteOriginRequestPolicy = DeleteOriginRequestPolicy'
  { -- | The version of the origin request policy that you are deleting. The
    -- version is the origin request policy’s @ETag@ value, which you can get
    -- using @ListOriginRequestPolicies@, @GetOriginRequestPolicy@, or
    -- @GetOriginRequestPolicyConfig@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the origin request policy that you are
    -- deleting. To get the identifier, you can use
    -- @ListOriginRequestPolicies@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOriginRequestPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteOriginRequestPolicy_ifMatch' - The version of the origin request policy that you are deleting. The
-- version is the origin request policy’s @ETag@ value, which you can get
-- using @ListOriginRequestPolicies@, @GetOriginRequestPolicy@, or
-- @GetOriginRequestPolicyConfig@.
--
-- 'id', 'deleteOriginRequestPolicy_id' - The unique identifier for the origin request policy that you are
-- deleting. To get the identifier, you can use
-- @ListOriginRequestPolicies@.
newDeleteOriginRequestPolicy ::
  -- | 'id'
  Prelude.Text ->
  DeleteOriginRequestPolicy
newDeleteOriginRequestPolicy pId_ =
  DeleteOriginRequestPolicy'
    { ifMatch =
        Prelude.Nothing,
      id = pId_
    }

-- | The version of the origin request policy that you are deleting. The
-- version is the origin request policy’s @ETag@ value, which you can get
-- using @ListOriginRequestPolicies@, @GetOriginRequestPolicy@, or
-- @GetOriginRequestPolicyConfig@.
deleteOriginRequestPolicy_ifMatch :: Lens.Lens' DeleteOriginRequestPolicy (Prelude.Maybe Prelude.Text)
deleteOriginRequestPolicy_ifMatch = Lens.lens (\DeleteOriginRequestPolicy' {ifMatch} -> ifMatch) (\s@DeleteOriginRequestPolicy' {} a -> s {ifMatch = a} :: DeleteOriginRequestPolicy)

-- | The unique identifier for the origin request policy that you are
-- deleting. To get the identifier, you can use
-- @ListOriginRequestPolicies@.
deleteOriginRequestPolicy_id :: Lens.Lens' DeleteOriginRequestPolicy Prelude.Text
deleteOriginRequestPolicy_id = Lens.lens (\DeleteOriginRequestPolicy' {id} -> id) (\s@DeleteOriginRequestPolicy' {} a -> s {id = a} :: DeleteOriginRequestPolicy)

instance Prelude.AWSRequest DeleteOriginRequestPolicy where
  type
    Rs DeleteOriginRequestPolicy =
      DeleteOriginRequestPolicyResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull
      DeleteOriginRequestPolicyResponse'

instance Prelude.Hashable DeleteOriginRequestPolicy

instance Prelude.NFData DeleteOriginRequestPolicy

instance Prelude.ToHeaders DeleteOriginRequestPolicy where
  toHeaders DeleteOriginRequestPolicy' {..} =
    Prelude.mconcat ["If-Match" Prelude.=# ifMatch]

instance Prelude.ToPath DeleteOriginRequestPolicy where
  toPath DeleteOriginRequestPolicy' {..} =
    Prelude.mconcat
      [ "/2020-05-31/origin-request-policy/",
        Prelude.toBS id
      ]

instance Prelude.ToQuery DeleteOriginRequestPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteOriginRequestPolicyResponse' smart constructor.
data DeleteOriginRequestPolicyResponse = DeleteOriginRequestPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteOriginRequestPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteOriginRequestPolicyResponse ::
  DeleteOriginRequestPolicyResponse
newDeleteOriginRequestPolicyResponse =
  DeleteOriginRequestPolicyResponse'

instance
  Prelude.NFData
    DeleteOriginRequestPolicyResponse
