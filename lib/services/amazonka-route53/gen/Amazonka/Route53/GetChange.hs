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
-- Module      : Amazonka.Route53.GetChange
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Route53.GetChange
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53.Types

-- | The input for a GetChange request.
--
-- /See:/ 'newGetChange' smart constructor.
data GetChange = GetChange'
  { -- | The ID of the change batch request. The value that you specify here is
    -- the value that @ChangeResourceRecordSets@ returned in the @Id@ element
    -- when you submitted the request.
    id :: Prelude.Text
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
  Prelude.Text ->
  GetChange
newGetChange pId_ = GetChange' {id = pId_}

-- | The ID of the change batch request. The value that you specify here is
-- the value that @ChangeResourceRecordSets@ returned in the @Id@ element
-- when you submitted the request.
getChange_id :: Lens.Lens' GetChange Prelude.Text
getChange_id = Lens.lens (\GetChange' {id} -> id) (\s@GetChange' {} a -> s {id = a} :: GetChange)

instance Core.AWSRequest GetChange where
  type AWSResponse GetChange = GetChangeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetChangeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "ChangeInfo")
      )

instance Prelude.Hashable GetChange where
  hashWithSalt _salt GetChange' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetChange where
  rnf GetChange' {..} = Prelude.rnf id

instance Data.ToHeaders GetChange where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetChange where
  toPath GetChange' {..} =
    Prelude.mconcat
      ["/2013-04-01/change/", Data.toBS id]

instance Data.ToQuery GetChange where
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

instance Prelude.NFData GetChangeResponse where
  rnf GetChangeResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf changeInfo
