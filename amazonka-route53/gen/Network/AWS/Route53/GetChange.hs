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
-- Module      : Network.AWS.Route53.GetChange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of a change batch request. The status is one
-- of the following values:
--
-- -   @PENDING@ indicates that the changes in this request have not
--     propagated to all Amazon Route 53 DNS servers. This is the initial
--     status of all change batch requests.
--
-- -   @INSYNC@ indicates that the changes have propagated to all Route 53
--     DNS servers.
module Network.AWS.Route53.GetChange
  ( -- * Creating a Request
    GetChange (..),
    newGetChange,

    -- * Request Lenses
    getChange_id,

    -- * Destructuring the Response
    GetChangeResponse (..),
    newGetChangeResponse,

    -- * Response Lenses
    getChangeResponse_httpStatus,
    getChangeResponse_changeInfo,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53.Types

-- | The input for a GetChange request.
--
-- /See:/ 'newGetChange' smart constructor.
data GetChange = GetChange'
  { -- | The ID of the change batch request. The value that you specify here is
    -- the value that @ChangeResourceRecordSets@ returned in the @Id@ element
    -- when you submitted the request.
    id :: ResourceId
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getChange_id' - The ID of the change batch request. The value that you specify here is
-- the value that @ChangeResourceRecordSets@ returned in the @Id@ element
-- when you submitted the request.
newGetChange ::
  -- | 'id'
  ResourceId ->
  GetChange
newGetChange pId_ = GetChange' {id = pId_}

-- | The ID of the change batch request. The value that you specify here is
-- the value that @ChangeResourceRecordSets@ returned in the @Id@ element
-- when you submitted the request.
getChange_id :: Lens.Lens' GetChange ResourceId
getChange_id = Lens.lens (\GetChange' {id} -> id) (\s@GetChange' {} a -> s {id = a} :: GetChange)

instance Core.AWSRequest GetChange where
  type AWSResponse GetChange = GetChangeResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetChangeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..@ "ChangeInfo")
      )

instance Prelude.Hashable GetChange

instance Prelude.NFData GetChange

instance Core.ToHeaders GetChange where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetChange where
  toPath GetChange' {..} =
    Prelude.mconcat
      ["/2013-04-01/change/", Core.toBS id]

instance Core.ToQuery GetChange where
  toQuery = Prelude.const Prelude.mempty

-- | A complex type that contains the @ChangeInfo@ element.
--
-- /See:/ 'newGetChangeResponse' smart constructor.
data GetChangeResponse = GetChangeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A complex type that contains information about the specified change
    -- batch.
    changeInfo :: ChangeInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetChangeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getChangeResponse_httpStatus' - The response's http status code.
--
-- 'changeInfo', 'getChangeResponse_changeInfo' - A complex type that contains information about the specified change
-- batch.
newGetChangeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'changeInfo'
  ChangeInfo ->
  GetChangeResponse
newGetChangeResponse pHttpStatus_ pChangeInfo_ =
  GetChangeResponse'
    { httpStatus = pHttpStatus_,
      changeInfo = pChangeInfo_
    }

-- | The response's http status code.
getChangeResponse_httpStatus :: Lens.Lens' GetChangeResponse Prelude.Int
getChangeResponse_httpStatus = Lens.lens (\GetChangeResponse' {httpStatus} -> httpStatus) (\s@GetChangeResponse' {} a -> s {httpStatus = a} :: GetChangeResponse)

-- | A complex type that contains information about the specified change
-- batch.
getChangeResponse_changeInfo :: Lens.Lens' GetChangeResponse ChangeInfo
getChangeResponse_changeInfo = Lens.lens (\GetChangeResponse' {changeInfo} -> changeInfo) (\s@GetChangeResponse' {} a -> s {changeInfo = a} :: GetChangeResponse)

instance Prelude.NFData GetChangeResponse
