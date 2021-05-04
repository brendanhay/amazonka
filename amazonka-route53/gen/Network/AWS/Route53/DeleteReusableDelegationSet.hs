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
-- Module      : Network.AWS.Route53.DeleteReusableDelegationSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reusable delegation set.
--
-- You can delete a reusable delegation set only if it isn\'t associated
-- with any hosted zones.
--
-- To verify that the reusable delegation set is not associated with any
-- hosted zones, submit a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GetReusableDelegationSet.html GetReusableDelegationSet>
-- request and specify the ID of the reusable delegation set that you want
-- to delete.
module Network.AWS.Route53.DeleteReusableDelegationSet
  ( -- * Creating a Request
    DeleteReusableDelegationSet (..),
    newDeleteReusableDelegationSet,

    -- * Request Lenses
    deleteReusableDelegationSet_id,

    -- * Destructuring the Response
    DeleteReusableDelegationSetResponse (..),
    newDeleteReusableDelegationSetResponse,

    -- * Response Lenses
    deleteReusableDelegationSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A request to delete a reusable delegation set.
--
-- /See:/ 'newDeleteReusableDelegationSet' smart constructor.
data DeleteReusableDelegationSet = DeleteReusableDelegationSet'
  { -- | The ID of the reusable delegation set that you want to delete.
    id :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteReusableDelegationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteReusableDelegationSet_id' - The ID of the reusable delegation set that you want to delete.
newDeleteReusableDelegationSet ::
  -- | 'id'
  ResourceId ->
  DeleteReusableDelegationSet
newDeleteReusableDelegationSet pId_ =
  DeleteReusableDelegationSet' {id = pId_}

-- | The ID of the reusable delegation set that you want to delete.
deleteReusableDelegationSet_id :: Lens.Lens' DeleteReusableDelegationSet ResourceId
deleteReusableDelegationSet_id = Lens.lens (\DeleteReusableDelegationSet' {id} -> id) (\s@DeleteReusableDelegationSet' {} a -> s {id = a} :: DeleteReusableDelegationSet)

instance
  Prelude.AWSRequest
    DeleteReusableDelegationSet
  where
  type
    Rs DeleteReusableDelegationSet =
      DeleteReusableDelegationSetResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteReusableDelegationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteReusableDelegationSet

instance Prelude.NFData DeleteReusableDelegationSet

instance
  Prelude.ToHeaders
    DeleteReusableDelegationSet
  where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteReusableDelegationSet where
  toPath DeleteReusableDelegationSet' {..} =
    Prelude.mconcat
      ["/2013-04-01/delegationset/", Prelude.toBS id]

instance Prelude.ToQuery DeleteReusableDelegationSet where
  toQuery = Prelude.const Prelude.mempty

-- | An empty element.
--
-- /See:/ 'newDeleteReusableDelegationSetResponse' smart constructor.
data DeleteReusableDelegationSetResponse = DeleteReusableDelegationSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteReusableDelegationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteReusableDelegationSetResponse_httpStatus' - The response's http status code.
newDeleteReusableDelegationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteReusableDelegationSetResponse
newDeleteReusableDelegationSetResponse pHttpStatus_ =
  DeleteReusableDelegationSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteReusableDelegationSetResponse_httpStatus :: Lens.Lens' DeleteReusableDelegationSetResponse Prelude.Int
deleteReusableDelegationSetResponse_httpStatus = Lens.lens (\DeleteReusableDelegationSetResponse' {httpStatus} -> httpStatus) (\s@DeleteReusableDelegationSetResponse' {} a -> s {httpStatus = a} :: DeleteReusableDelegationSetResponse)

instance
  Prelude.NFData
    DeleteReusableDelegationSetResponse
