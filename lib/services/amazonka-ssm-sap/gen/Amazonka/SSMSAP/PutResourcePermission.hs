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
-- Module      : Amazonka.SSMSAP.PutResourcePermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds permissions to the target database.
module Amazonka.SSMSAP.PutResourcePermission
  ( -- * Creating a Request
    PutResourcePermission (..),
    newPutResourcePermission,

    -- * Request Lenses
    putResourcePermission_actionType,
    putResourcePermission_sourceResourceArn,
    putResourcePermission_resourceArn,

    -- * Destructuring the Response
    PutResourcePermissionResponse (..),
    newPutResourcePermissionResponse,

    -- * Response Lenses
    putResourcePermissionResponse_policy,
    putResourcePermissionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMSAP.Types

-- | /See:/ 'newPutResourcePermission' smart constructor.
data PutResourcePermission = PutResourcePermission'
  { actionType :: PermissionActionType,
    sourceResourceArn :: Prelude.Text,
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionType', 'putResourcePermission_actionType' -
--
-- 'sourceResourceArn', 'putResourcePermission_sourceResourceArn' -
--
-- 'resourceArn', 'putResourcePermission_resourceArn' -
newPutResourcePermission ::
  -- | 'actionType'
  PermissionActionType ->
  -- | 'sourceResourceArn'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  PutResourcePermission
newPutResourcePermission
  pActionType_
  pSourceResourceArn_
  pResourceArn_ =
    PutResourcePermission'
      { actionType = pActionType_,
        sourceResourceArn = pSourceResourceArn_,
        resourceArn = pResourceArn_
      }

-- |
putResourcePermission_actionType :: Lens.Lens' PutResourcePermission PermissionActionType
putResourcePermission_actionType = Lens.lens (\PutResourcePermission' {actionType} -> actionType) (\s@PutResourcePermission' {} a -> s {actionType = a} :: PutResourcePermission)

-- |
putResourcePermission_sourceResourceArn :: Lens.Lens' PutResourcePermission Prelude.Text
putResourcePermission_sourceResourceArn = Lens.lens (\PutResourcePermission' {sourceResourceArn} -> sourceResourceArn) (\s@PutResourcePermission' {} a -> s {sourceResourceArn = a} :: PutResourcePermission)

-- |
putResourcePermission_resourceArn :: Lens.Lens' PutResourcePermission Prelude.Text
putResourcePermission_resourceArn = Lens.lens (\PutResourcePermission' {resourceArn} -> resourceArn) (\s@PutResourcePermission' {} a -> s {resourceArn = a} :: PutResourcePermission)

instance Core.AWSRequest PutResourcePermission where
  type
    AWSResponse PutResourcePermission =
      PutResourcePermissionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePermissionResponse'
            Prelude.<$> (x Core..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePermission where
  hashWithSalt _salt PutResourcePermission' {..} =
    _salt `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` sourceResourceArn
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData PutResourcePermission where
  rnf PutResourcePermission' {..} =
    Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf sourceResourceArn
      `Prelude.seq` Prelude.rnf resourceArn

instance Core.ToHeaders PutResourcePermission where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutResourcePermission where
  toJSON PutResourcePermission' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ActionType" Core..= actionType),
            Prelude.Just
              ("SourceResourceArn" Core..= sourceResourceArn),
            Prelude.Just ("ResourceArn" Core..= resourceArn)
          ]
      )

instance Core.ToPath PutResourcePermission where
  toPath = Prelude.const "/put-resource-permission"

instance Core.ToQuery PutResourcePermission where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePermissionResponse' smart constructor.
data PutResourcePermissionResponse = PutResourcePermissionResponse'
  { policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePermissionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policy', 'putResourcePermissionResponse_policy' -
--
-- 'httpStatus', 'putResourcePermissionResponse_httpStatus' - The response's http status code.
newPutResourcePermissionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePermissionResponse
newPutResourcePermissionResponse pHttpStatus_ =
  PutResourcePermissionResponse'
    { policy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- |
putResourcePermissionResponse_policy :: Lens.Lens' PutResourcePermissionResponse (Prelude.Maybe Prelude.Text)
putResourcePermissionResponse_policy = Lens.lens (\PutResourcePermissionResponse' {policy} -> policy) (\s@PutResourcePermissionResponse' {} a -> s {policy = a} :: PutResourcePermissionResponse)

-- | The response's http status code.
putResourcePermissionResponse_httpStatus :: Lens.Lens' PutResourcePermissionResponse Prelude.Int
putResourcePermissionResponse_httpStatus = Lens.lens (\PutResourcePermissionResponse' {httpStatus} -> httpStatus) (\s@PutResourcePermissionResponse' {} a -> s {httpStatus = a} :: PutResourcePermissionResponse)

instance Prelude.NFData PutResourcePermissionResponse where
  rnf PutResourcePermissionResponse' {..} =
    Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
