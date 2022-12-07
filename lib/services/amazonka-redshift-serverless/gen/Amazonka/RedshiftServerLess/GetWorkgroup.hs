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
-- Module      : Amazonka.RedshiftServerLess.GetWorkgroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specific workgroup.
module Amazonka.RedshiftServerLess.GetWorkgroup
  ( -- * Creating a Request
    GetWorkgroup (..),
    newGetWorkgroup,

    -- * Request Lenses
    getWorkgroup_workgroupName,

    -- * Destructuring the Response
    GetWorkgroupResponse (..),
    newGetWorkgroupResponse,

    -- * Response Lenses
    getWorkgroupResponse_httpStatus,
    getWorkgroupResponse_workgroup,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkgroup' smart constructor.
data GetWorkgroup = GetWorkgroup'
  { -- | The name of the workgroup to return information for.
    workgroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkgroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workgroupName', 'getWorkgroup_workgroupName' - The name of the workgroup to return information for.
newGetWorkgroup ::
  -- | 'workgroupName'
  Prelude.Text ->
  GetWorkgroup
newGetWorkgroup pWorkgroupName_ =
  GetWorkgroup' {workgroupName = pWorkgroupName_}

-- | The name of the workgroup to return information for.
getWorkgroup_workgroupName :: Lens.Lens' GetWorkgroup Prelude.Text
getWorkgroup_workgroupName = Lens.lens (\GetWorkgroup' {workgroupName} -> workgroupName) (\s@GetWorkgroup' {} a -> s {workgroupName = a} :: GetWorkgroup)

instance Core.AWSRequest GetWorkgroup where
  type AWSResponse GetWorkgroup = GetWorkgroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkgroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workgroup")
      )

instance Prelude.Hashable GetWorkgroup where
  hashWithSalt _salt GetWorkgroup' {..} =
    _salt `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData GetWorkgroup where
  rnf GetWorkgroup' {..} = Prelude.rnf workgroupName

instance Data.ToHeaders GetWorkgroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.GetWorkgroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetWorkgroup where
  toJSON GetWorkgroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("workgroupName" Data..= workgroupName)
          ]
      )

instance Data.ToPath GetWorkgroup where
  toPath = Prelude.const "/"

instance Data.ToQuery GetWorkgroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkgroupResponse' smart constructor.
data GetWorkgroupResponse = GetWorkgroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The returned workgroup object.
    workgroup :: Workgroup
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkgroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getWorkgroupResponse_httpStatus' - The response's http status code.
--
-- 'workgroup', 'getWorkgroupResponse_workgroup' - The returned workgroup object.
newGetWorkgroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workgroup'
  Workgroup ->
  GetWorkgroupResponse
newGetWorkgroupResponse pHttpStatus_ pWorkgroup_ =
  GetWorkgroupResponse'
    { httpStatus = pHttpStatus_,
      workgroup = pWorkgroup_
    }

-- | The response's http status code.
getWorkgroupResponse_httpStatus :: Lens.Lens' GetWorkgroupResponse Prelude.Int
getWorkgroupResponse_httpStatus = Lens.lens (\GetWorkgroupResponse' {httpStatus} -> httpStatus) (\s@GetWorkgroupResponse' {} a -> s {httpStatus = a} :: GetWorkgroupResponse)

-- | The returned workgroup object.
getWorkgroupResponse_workgroup :: Lens.Lens' GetWorkgroupResponse Workgroup
getWorkgroupResponse_workgroup = Lens.lens (\GetWorkgroupResponse' {workgroup} -> workgroup) (\s@GetWorkgroupResponse' {} a -> s {workgroup = a} :: GetWorkgroupResponse)

instance Prelude.NFData GetWorkgroupResponse where
  rnf GetWorkgroupResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workgroup
