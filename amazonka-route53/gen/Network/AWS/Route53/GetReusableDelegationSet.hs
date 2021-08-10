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
-- Module      : Network.AWS.Route53.GetReusableDelegationSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a specified reusable delegation set,
-- including the four name servers that are assigned to the delegation set.
module Network.AWS.Route53.GetReusableDelegationSet
  ( -- * Creating a Request
    GetReusableDelegationSet (..),
    newGetReusableDelegationSet,

    -- * Request Lenses
    getReusableDelegationSet_id,

    -- * Destructuring the Response
    GetReusableDelegationSetResponse (..),
    newGetReusableDelegationSetResponse,

    -- * Response Lenses
    getReusableDelegationSetResponse_httpStatus,
    getReusableDelegationSetResponse_delegationSet,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | A request to get information about a specified reusable delegation set.
--
-- /See:/ 'newGetReusableDelegationSet' smart constructor.
data GetReusableDelegationSet = GetReusableDelegationSet'
  { -- | The ID of the reusable delegation set that you want to get a list of
    -- name servers for.
    id :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReusableDelegationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getReusableDelegationSet_id' - The ID of the reusable delegation set that you want to get a list of
-- name servers for.
newGetReusableDelegationSet ::
  -- | 'id'
  ResourceId ->
  GetReusableDelegationSet
newGetReusableDelegationSet pId_ =
  GetReusableDelegationSet' {id = pId_}

-- | The ID of the reusable delegation set that you want to get a list of
-- name servers for.
getReusableDelegationSet_id :: Lens.Lens' GetReusableDelegationSet ResourceId
getReusableDelegationSet_id = Lens.lens (\GetReusableDelegationSet' {id} -> id) (\s@GetReusableDelegationSet' {} a -> s {id = a} :: GetReusableDelegationSet)

instance Core.AWSRequest GetReusableDelegationSet where
  type
    AWSResponse GetReusableDelegationSet =
      GetReusableDelegationSetResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetReusableDelegationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "DelegationSet")
      )

instance Prelude.Hashable GetReusableDelegationSet

instance Prelude.NFData GetReusableDelegationSet

instance Core.ToHeaders GetReusableDelegationSet where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetReusableDelegationSet where
  toPath GetReusableDelegationSet' {..} =
    Prelude.mconcat
      ["/2013-04-01/delegationset/", Core.toBS id]

instance Core.ToQuery GetReusableDelegationSet where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the response to the
-- @GetReusableDelegationSet@ request.
--
-- /See:/ 'newGetReusableDelegationSetResponse' smart constructor.
data GetReusableDelegationSetResponse = GetReusableDelegationSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains information about the reusable delegation
    -- set.
    delegationSet :: DelegationSet
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetReusableDelegationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getReusableDelegationSetResponse_httpStatus' - The response's http status code.
--
-- 'delegationSet', 'getReusableDelegationSetResponse_delegationSet' - A complex type that contains information about the reusable delegation
-- set.
newGetReusableDelegationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'delegationSet'
  DelegationSet ->
  GetReusableDelegationSetResponse
newGetReusableDelegationSetResponse
  pHttpStatus_
  pDelegationSet_ =
    GetReusableDelegationSetResponse'
      { httpStatus =
          pHttpStatus_,
        delegationSet = pDelegationSet_
      }

-- | The response's http status code.
getReusableDelegationSetResponse_httpStatus :: Lens.Lens' GetReusableDelegationSetResponse Prelude.Int
getReusableDelegationSetResponse_httpStatus = Lens.lens (\GetReusableDelegationSetResponse' {httpStatus} -> httpStatus) (\s@GetReusableDelegationSetResponse' {} a -> s {httpStatus = a} :: GetReusableDelegationSetResponse)

-- | A complex type that contains information about the reusable delegation
-- set.
getReusableDelegationSetResponse_delegationSet :: Lens.Lens' GetReusableDelegationSetResponse DelegationSet
getReusableDelegationSetResponse_delegationSet = Lens.lens (\GetReusableDelegationSetResponse' {delegationSet} -> delegationSet) (\s@GetReusableDelegationSetResponse' {} a -> s {delegationSet = a} :: GetReusableDelegationSetResponse)

instance
  Prelude.NFData
    GetReusableDelegationSetResponse
