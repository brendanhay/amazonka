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
-- Module      : Amazonka.CloudFront.DeleteResponseHeadersPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a response headers policy.
--
-- You cannot delete a response headers policy if it’s attached to a cache
-- behavior. First update your distributions to remove the response headers
-- policy from all cache behaviors, then delete the response headers
-- policy.
--
-- To delete a response headers policy, you must provide the policy’s
-- identifier and version. To get these values, you can use
-- @ListResponseHeadersPolicies@ or @GetResponseHeadersPolicy@.
module Amazonka.CloudFront.DeleteResponseHeadersPolicy
  ( -- * Creating a Request
    DeleteResponseHeadersPolicy (..),
    newDeleteResponseHeadersPolicy,

    -- * Request Lenses
    deleteResponseHeadersPolicy_ifMatch,
    deleteResponseHeadersPolicy_id,

    -- * Destructuring the Response
    DeleteResponseHeadersPolicyResponse (..),
    newDeleteResponseHeadersPolicyResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteResponseHeadersPolicy' smart constructor.
data DeleteResponseHeadersPolicy = DeleteResponseHeadersPolicy'
  { -- | The version of the response headers policy that you are deleting.
    --
    -- The version is the response headers policy’s @ETag@ value, which you can
    -- get using @ListResponseHeadersPolicies@, @GetResponseHeadersPolicy@, or
    -- @GetResponseHeadersPolicyConfig@.
    ifMatch :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the response headers policy that you are deleting.
    --
    -- To get the identifier, you can use @ListResponseHeadersPolicies@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResponseHeadersPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ifMatch', 'deleteResponseHeadersPolicy_ifMatch' - The version of the response headers policy that you are deleting.
--
-- The version is the response headers policy’s @ETag@ value, which you can
-- get using @ListResponseHeadersPolicies@, @GetResponseHeadersPolicy@, or
-- @GetResponseHeadersPolicyConfig@.
--
-- 'id', 'deleteResponseHeadersPolicy_id' - The identifier for the response headers policy that you are deleting.
--
-- To get the identifier, you can use @ListResponseHeadersPolicies@.
newDeleteResponseHeadersPolicy ::
  -- | 'id'
  Prelude.Text ->
  DeleteResponseHeadersPolicy
newDeleteResponseHeadersPolicy pId_ =
  DeleteResponseHeadersPolicy'
    { ifMatch =
        Prelude.Nothing,
      id = pId_
    }

-- | The version of the response headers policy that you are deleting.
--
-- The version is the response headers policy’s @ETag@ value, which you can
-- get using @ListResponseHeadersPolicies@, @GetResponseHeadersPolicy@, or
-- @GetResponseHeadersPolicyConfig@.
deleteResponseHeadersPolicy_ifMatch :: Lens.Lens' DeleteResponseHeadersPolicy (Prelude.Maybe Prelude.Text)
deleteResponseHeadersPolicy_ifMatch = Lens.lens (\DeleteResponseHeadersPolicy' {ifMatch} -> ifMatch) (\s@DeleteResponseHeadersPolicy' {} a -> s {ifMatch = a} :: DeleteResponseHeadersPolicy)

-- | The identifier for the response headers policy that you are deleting.
--
-- To get the identifier, you can use @ListResponseHeadersPolicies@.
deleteResponseHeadersPolicy_id :: Lens.Lens' DeleteResponseHeadersPolicy Prelude.Text
deleteResponseHeadersPolicy_id = Lens.lens (\DeleteResponseHeadersPolicy' {id} -> id) (\s@DeleteResponseHeadersPolicy' {} a -> s {id = a} :: DeleteResponseHeadersPolicy)

instance Core.AWSRequest DeleteResponseHeadersPolicy where
  type
    AWSResponse DeleteResponseHeadersPolicy =
      DeleteResponseHeadersPolicyResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteResponseHeadersPolicyResponse'

instance Prelude.Hashable DeleteResponseHeadersPolicy where
  hashWithSalt _salt DeleteResponseHeadersPolicy' {..} =
    _salt `Prelude.hashWithSalt` ifMatch
      `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteResponseHeadersPolicy where
  rnf DeleteResponseHeadersPolicy' {..} =
    Prelude.rnf ifMatch `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders DeleteResponseHeadersPolicy where
  toHeaders DeleteResponseHeadersPolicy' {..} =
    Prelude.mconcat ["If-Match" Data.=# ifMatch]

instance Data.ToPath DeleteResponseHeadersPolicy where
  toPath DeleteResponseHeadersPolicy' {..} =
    Prelude.mconcat
      [ "/2020-05-31/response-headers-policy/",
        Data.toBS id
      ]

instance Data.ToQuery DeleteResponseHeadersPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteResponseHeadersPolicyResponse' smart constructor.
data DeleteResponseHeadersPolicyResponse = DeleteResponseHeadersPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteResponseHeadersPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteResponseHeadersPolicyResponse ::
  DeleteResponseHeadersPolicyResponse
newDeleteResponseHeadersPolicyResponse =
  DeleteResponseHeadersPolicyResponse'

instance
  Prelude.NFData
    DeleteResponseHeadersPolicyResponse
  where
  rnf _ = ()
