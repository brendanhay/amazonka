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
-- Module      : Amazonka.Athena.GetWorkGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the workgroup with the specified name.
module Amazonka.Athena.GetWorkGroup
  ( -- * Creating a Request
    GetWorkGroup (..),
    newGetWorkGroup,

    -- * Request Lenses
    getWorkGroup_workGroup,

    -- * Destructuring the Response
    GetWorkGroupResponse (..),
    newGetWorkGroupResponse,

    -- * Response Lenses
    getWorkGroupResponse_workGroup,
    getWorkGroupResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkGroup' smart constructor.
data GetWorkGroup = GetWorkGroup'
  { -- | The name of the workgroup.
    workGroup :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workGroup', 'getWorkGroup_workGroup' - The name of the workgroup.
newGetWorkGroup ::
  -- | 'workGroup'
  Prelude.Text ->
  GetWorkGroup
newGetWorkGroup pWorkGroup_ =
  GetWorkGroup' {workGroup = pWorkGroup_}

-- | The name of the workgroup.
getWorkGroup_workGroup :: Lens.Lens' GetWorkGroup Prelude.Text
getWorkGroup_workGroup = Lens.lens (\GetWorkGroup' {workGroup} -> workGroup) (\s@GetWorkGroup' {} a -> s {workGroup = a} :: GetWorkGroup)

instance Core.AWSRequest GetWorkGroup where
  type AWSResponse GetWorkGroup = GetWorkGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkGroupResponse'
            Prelude.<$> (x Data..?> "WorkGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkGroup where
  hashWithSalt _salt GetWorkGroup' {..} =
    _salt `Prelude.hashWithSalt` workGroup

instance Prelude.NFData GetWorkGroup where
  rnf GetWorkGroup' {..} = Prelude.rnf workGroup

instance Data.ToHeaders GetWorkGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonAthena.GetWorkGroup" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetWorkGroup where
  toJSON GetWorkGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("WorkGroup" Data..= workGroup)]
      )

instance Data.ToPath GetWorkGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery GetWorkGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWorkGroupResponse' smart constructor.
data GetWorkGroupResponse = GetWorkGroupResponse'
  { -- | Information about the workgroup.
    workGroup :: Prelude.Maybe WorkGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workGroup', 'getWorkGroupResponse_workGroup' - Information about the workgroup.
--
-- 'httpStatus', 'getWorkGroupResponse_httpStatus' - The response's http status code.
newGetWorkGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkGroupResponse
newGetWorkGroupResponse pHttpStatus_ =
  GetWorkGroupResponse'
    { workGroup = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the workgroup.
getWorkGroupResponse_workGroup :: Lens.Lens' GetWorkGroupResponse (Prelude.Maybe WorkGroup)
getWorkGroupResponse_workGroup = Lens.lens (\GetWorkGroupResponse' {workGroup} -> workGroup) (\s@GetWorkGroupResponse' {} a -> s {workGroup = a} :: GetWorkGroupResponse)

-- | The response's http status code.
getWorkGroupResponse_httpStatus :: Lens.Lens' GetWorkGroupResponse Prelude.Int
getWorkGroupResponse_httpStatus = Lens.lens (\GetWorkGroupResponse' {httpStatus} -> httpStatus) (\s@GetWorkGroupResponse' {} a -> s {httpStatus = a} :: GetWorkGroupResponse)

instance Prelude.NFData GetWorkGroupResponse where
  rnf GetWorkGroupResponse' {..} =
    Prelude.rnf workGroup `Prelude.seq`
      Prelude.rnf httpStatus
