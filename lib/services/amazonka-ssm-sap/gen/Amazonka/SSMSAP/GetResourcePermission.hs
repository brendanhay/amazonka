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
-- Module      : Amazonka.SSMSAP.GetResourcePermission
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets permissions associated with the target database.
module Amazonka.SSMSAP.GetResourcePermission
  ( -- * Creating a Request
    GetResourcePermission (..),
    newGetResourcePermission,

    -- * Request Lenses
    getResourcePermission_actionType,
    getResourcePermission_resourceArn,

    -- * Destructuring the Response
    GetResourcePermissionResponse (..),
    newGetResourcePermissionResponse,

    -- * Response Lenses
    getResourcePermissionResponse_policy,
    getResourcePermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newGetResourcePermission' smart constructor.
data GetResourcePermission = GetResourcePermission'
  { actionType :: Prelude.Maybe PermissionActionType,
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'getResourcePermission_actionType' -
--
-- 'resourceArn', 'getResourcePermission_resourceArn' -
newGetResourcePermission ::
  -- | 'resourceArn'
  Prelude.Text ->
  GetResourcePermission
newGetResourcePermission pResourceArn_ =
  GetResourcePermission'
    { actionType =
        Prelude.Nothing,
      resourceArn = pResourceArn_
    }

getResourcePermission_actionType :: Lens.Lens' GetResourcePermission (Prelude.Maybe PermissionActionType)
getResourcePermission_actionType = Lens.lens (\GetResourcePermission' {actionType} -> actionType) (\s@GetResourcePermission' {} a -> s {actionType = a} :: GetResourcePermission)

getResourcePermission_resourceArn :: Lens.Lens' GetResourcePermission Prelude.Text
getResourcePermission_resourceArn = Lens.lens (\GetResourcePermission' {resourceArn} -> resourceArn) (\s@GetResourcePermission' {} a -> s {resourceArn = a} :: GetResourcePermission)

instance Core.AWSRequest GetResourcePermission where
  type
    AWSResponse GetResourcePermission =
      GetResourcePermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePermissionResponse'
            Prelude.<$> (x Data..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePermission where
  hashWithSalt _salt GetResourcePermission' {..} =
    _salt
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData GetResourcePermission where
  rnf GetResourcePermission' {..} =
    Prelude.rnf actionType `Prelude.seq`
      Prelude.rnf resourceArn

instance Data.ToHeaders GetResourcePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourcePermission where
  toJSON GetResourcePermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActionType" Data..=) Prelude.<$> actionType,
            Prelude.Just ("ResourceArn" Data..= resourceArn)
          ]
      )

instance Data.ToPath GetResourcePermission where
  toPath = Prelude.const "/get-resource-permission"

instance Data.ToQuery GetResourcePermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcePermissionResponse' smart constructor.
data GetResourcePermissionResponse = GetResourcePermissionResponse'
  { policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'getResourcePermissionResponse_policy' -
--
-- 'httpStatus', 'getResourcePermissionResponse_httpStatus' - The response's http status code.
newGetResourcePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcePermissionResponse
newGetResourcePermissionResponse pHttpStatus_ =
  GetResourcePermissionResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

getResourcePermissionResponse_policy :: Lens.Lens' GetResourcePermissionResponse (Prelude.Maybe Prelude.Text)
getResourcePermissionResponse_policy = Lens.lens (\GetResourcePermissionResponse' {policy} -> policy) (\s@GetResourcePermissionResponse' {} a -> s {policy = a} :: GetResourcePermissionResponse)

-- | The response's http status code.
getResourcePermissionResponse_httpStatus :: Lens.Lens' GetResourcePermissionResponse Prelude.Int
getResourcePermissionResponse_httpStatus = Lens.lens (\GetResourcePermissionResponse' {httpStatus} -> httpStatus) (\s@GetResourcePermissionResponse' {} a -> s {httpStatus = a} :: GetResourcePermissionResponse)

instance Prelude.NFData GetResourcePermissionResponse where
  rnf GetResourcePermissionResponse' {..} =
    Prelude.rnf policy `Prelude.seq`
      Prelude.rnf httpStatus
