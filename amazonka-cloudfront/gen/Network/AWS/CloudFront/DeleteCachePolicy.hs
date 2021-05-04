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
-- Module      : Network.AWS.CloudFront.DeleteCachePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache policy.
--
-- You cannot delete a cache policy if it’s attached to a cache behavior.
-- First update your distributions to remove the cache policy from all
-- cache behaviors, then delete the cache policy.
--
-- To delete a cache policy, you must provide the policy’s identifier and
-- version. To get these values, you can use @ListCachePolicies@ or
-- @GetCachePolicy@.
module Network.AWS.CloudFront.DeleteCachePolicy
  ( -- * Creating a Request
    DeleteCachePolicy (..),
    newDeleteCachePolicy,

    -- * Request Lenses
    deleteCachePolicy_ifMatch,
    deleteCachePolicy_id,

    -- * Destructuring the Response
    DeleteCachePolicyResponse (..),
    newDeleteCachePolicyResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCachePolicy' smart constructor.
data DeleteCachePolicy = DeleteCachePolicy'
  { -- | The version of the cache policy that you are deleting. The version is
    -- the cache policy’s @ETag@ value, which you can get using
    -- @ListCachePolicies@, @GetCachePolicy@, or @GetCachePolicyConfig@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the cache policy that you are deleting. To get
    -- the identifier, you can use @ListCachePolicies@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteCachePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteCachePolicy_ifMatch' - The version of the cache policy that you are deleting. The version is
-- the cache policy’s @ETag@ value, which you can get using
-- @ListCachePolicies@, @GetCachePolicy@, or @GetCachePolicyConfig@.
--
-- 'id', 'deleteCachePolicy_id' - The unique identifier for the cache policy that you are deleting. To get
-- the identifier, you can use @ListCachePolicies@.
newDeleteCachePolicy ::
  -- | 'id'
  Prelude.Text ->
  DeleteCachePolicy
newDeleteCachePolicy pId_ =
  DeleteCachePolicy'
    { ifMatch = Prelude.Nothing,
      id = pId_
    }

-- | The version of the cache policy that you are deleting. The version is
-- the cache policy’s @ETag@ value, which you can get using
-- @ListCachePolicies@, @GetCachePolicy@, or @GetCachePolicyConfig@.
deleteCachePolicy_ifMatch :: Lens.Lens' DeleteCachePolicy (Prelude.Maybe Prelude.Text)
deleteCachePolicy_ifMatch = Lens.lens (\DeleteCachePolicy' {ifMatch} -> ifMatch) (\s@DeleteCachePolicy' {} a -> s {ifMatch = a} :: DeleteCachePolicy)

-- | The unique identifier for the cache policy that you are deleting. To get
-- the identifier, you can use @ListCachePolicies@.
deleteCachePolicy_id :: Lens.Lens' DeleteCachePolicy Prelude.Text
deleteCachePolicy_id = Lens.lens (\DeleteCachePolicy' {id} -> id) (\s@DeleteCachePolicy' {} a -> s {id = a} :: DeleteCachePolicy)

instance Prelude.AWSRequest DeleteCachePolicy where
  type Rs DeleteCachePolicy = DeleteCachePolicyResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteCachePolicyResponse'

instance Prelude.Hashable DeleteCachePolicy

instance Prelude.NFData DeleteCachePolicy

instance Prelude.ToHeaders DeleteCachePolicy where
  toHeaders DeleteCachePolicy' {..} =
    Prelude.mconcat ["If-Match" Prelude.=# ifMatch]

instance Prelude.ToPath DeleteCachePolicy where
  toPath DeleteCachePolicy' {..} =
    Prelude.mconcat
      ["/2020-05-31/cache-policy/", Prelude.toBS id]

instance Prelude.ToQuery DeleteCachePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCachePolicyResponse' smart constructor.
data DeleteCachePolicyResponse = DeleteCachePolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteCachePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCachePolicyResponse ::
  DeleteCachePolicyResponse
newDeleteCachePolicyResponse =
  DeleteCachePolicyResponse'

instance Prelude.NFData DeleteCachePolicyResponse
