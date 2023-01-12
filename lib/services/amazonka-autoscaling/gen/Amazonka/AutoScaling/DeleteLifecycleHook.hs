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
-- Module      : Amazonka.AutoScaling.DeleteLifecycleHook
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified lifecycle hook.
--
-- If there are any outstanding lifecycle actions, they are completed first
-- (@ABANDON@ for launching instances, @CONTINUE@ for terminating
-- instances).
module Amazonka.AutoScaling.DeleteLifecycleHook
  ( -- * Creating a Request
    DeleteLifecycleHook (..),
    newDeleteLifecycleHook,

    -- * Request Lenses
    deleteLifecycleHook_lifecycleHookName,
    deleteLifecycleHook_autoScalingGroupName,

    -- * Destructuring the Response
    DeleteLifecycleHookResponse (..),
    newDeleteLifecycleHookResponse,

    -- * Response Lenses
    deleteLifecycleHookResponse_httpStatus,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLifecycleHook' smart constructor.
data DeleteLifecycleHook = DeleteLifecycleHook'
  { -- | The name of the lifecycle hook.
    lifecycleHookName :: Prelude.Text,
    -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLifecycleHook' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycleHookName', 'deleteLifecycleHook_lifecycleHookName' - The name of the lifecycle hook.
--
-- 'autoScalingGroupName', 'deleteLifecycleHook_autoScalingGroupName' - The name of the Auto Scaling group.
newDeleteLifecycleHook ::
  -- | 'lifecycleHookName'
  Prelude.Text ->
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  DeleteLifecycleHook
newDeleteLifecycleHook
  pLifecycleHookName_
  pAutoScalingGroupName_ =
    DeleteLifecycleHook'
      { lifecycleHookName =
          pLifecycleHookName_,
        autoScalingGroupName = pAutoScalingGroupName_
      }

-- | The name of the lifecycle hook.
deleteLifecycleHook_lifecycleHookName :: Lens.Lens' DeleteLifecycleHook Prelude.Text
deleteLifecycleHook_lifecycleHookName = Lens.lens (\DeleteLifecycleHook' {lifecycleHookName} -> lifecycleHookName) (\s@DeleteLifecycleHook' {} a -> s {lifecycleHookName = a} :: DeleteLifecycleHook)

-- | The name of the Auto Scaling group.
deleteLifecycleHook_autoScalingGroupName :: Lens.Lens' DeleteLifecycleHook Prelude.Text
deleteLifecycleHook_autoScalingGroupName = Lens.lens (\DeleteLifecycleHook' {autoScalingGroupName} -> autoScalingGroupName) (\s@DeleteLifecycleHook' {} a -> s {autoScalingGroupName = a} :: DeleteLifecycleHook)

instance Core.AWSRequest DeleteLifecycleHook where
  type
    AWSResponse DeleteLifecycleHook =
      DeleteLifecycleHookResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteLifecycleHookResult"
      ( \s h x ->
          DeleteLifecycleHookResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLifecycleHook where
  hashWithSalt _salt DeleteLifecycleHook' {..} =
    _salt `Prelude.hashWithSalt` lifecycleHookName
      `Prelude.hashWithSalt` autoScalingGroupName

instance Prelude.NFData DeleteLifecycleHook where
  rnf DeleteLifecycleHook' {..} =
    Prelude.rnf lifecycleHookName
      `Prelude.seq` Prelude.rnf autoScalingGroupName

instance Data.ToHeaders DeleteLifecycleHook where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteLifecycleHook where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLifecycleHook where
  toQuery DeleteLifecycleHook' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteLifecycleHook" :: Prelude.ByteString),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "LifecycleHookName" Data.=: lifecycleHookName,
        "AutoScalingGroupName" Data.=: autoScalingGroupName
      ]

-- | /See:/ 'newDeleteLifecycleHookResponse' smart constructor.
data DeleteLifecycleHookResponse = DeleteLifecycleHookResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLifecycleHookResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLifecycleHookResponse_httpStatus' - The response's http status code.
newDeleteLifecycleHookResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLifecycleHookResponse
newDeleteLifecycleHookResponse pHttpStatus_ =
  DeleteLifecycleHookResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLifecycleHookResponse_httpStatus :: Lens.Lens' DeleteLifecycleHookResponse Prelude.Int
deleteLifecycleHookResponse_httpStatus = Lens.lens (\DeleteLifecycleHookResponse' {httpStatus} -> httpStatus) (\s@DeleteLifecycleHookResponse' {} a -> s {httpStatus = a} :: DeleteLifecycleHookResponse)

instance Prelude.NFData DeleteLifecycleHookResponse where
  rnf DeleteLifecycleHookResponse' {..} =
    Prelude.rnf httpStatus
