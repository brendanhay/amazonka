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
-- Module      : Amazonka.CodeStarNotifications.DeleteTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified target for notifications.
module Amazonka.CodeStarNotifications.DeleteTarget
  ( -- * Creating a Request
    DeleteTarget (..),
    newDeleteTarget,

    -- * Request Lenses
    deleteTarget_forceUnsubscribeAll,
    deleteTarget_targetAddress,

    -- * Destructuring the Response
    DeleteTargetResponse (..),
    newDeleteTargetResponse,

    -- * Response Lenses
    deleteTargetResponse_httpStatus,
  )
where

import Amazonka.CodeStarNotifications.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTarget' smart constructor.
data DeleteTarget = DeleteTarget'
  { -- | A Boolean value that can be used to delete all associations with this
    -- Chatbot topic. The default value is FALSE. If set to TRUE, all
    -- associations between that target and every notification rule in your
    -- Amazon Web Services account are deleted.
    forceUnsubscribeAll :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Chatbot topic or Chatbot client to
    -- delete.
    targetAddress :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forceUnsubscribeAll', 'deleteTarget_forceUnsubscribeAll' - A Boolean value that can be used to delete all associations with this
-- Chatbot topic. The default value is FALSE. If set to TRUE, all
-- associations between that target and every notification rule in your
-- Amazon Web Services account are deleted.
--
-- 'targetAddress', 'deleteTarget_targetAddress' - The Amazon Resource Name (ARN) of the Chatbot topic or Chatbot client to
-- delete.
newDeleteTarget ::
  -- | 'targetAddress'
  Prelude.Text ->
  DeleteTarget
newDeleteTarget pTargetAddress_ =
  DeleteTarget'
    { forceUnsubscribeAll =
        Prelude.Nothing,
      targetAddress =
        Core._Sensitive Lens.# pTargetAddress_
    }

-- | A Boolean value that can be used to delete all associations with this
-- Chatbot topic. The default value is FALSE. If set to TRUE, all
-- associations between that target and every notification rule in your
-- Amazon Web Services account are deleted.
deleteTarget_forceUnsubscribeAll :: Lens.Lens' DeleteTarget (Prelude.Maybe Prelude.Bool)
deleteTarget_forceUnsubscribeAll = Lens.lens (\DeleteTarget' {forceUnsubscribeAll} -> forceUnsubscribeAll) (\s@DeleteTarget' {} a -> s {forceUnsubscribeAll = a} :: DeleteTarget)

-- | The Amazon Resource Name (ARN) of the Chatbot topic or Chatbot client to
-- delete.
deleteTarget_targetAddress :: Lens.Lens' DeleteTarget Prelude.Text
deleteTarget_targetAddress = Lens.lens (\DeleteTarget' {targetAddress} -> targetAddress) (\s@DeleteTarget' {} a -> s {targetAddress = a} :: DeleteTarget) Prelude.. Core._Sensitive

instance Core.AWSRequest DeleteTarget where
  type AWSResponse DeleteTarget = DeleteTargetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTargetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTarget where
  hashWithSalt _salt DeleteTarget' {..} =
    _salt `Prelude.hashWithSalt` forceUnsubscribeAll
      `Prelude.hashWithSalt` targetAddress

instance Prelude.NFData DeleteTarget where
  rnf DeleteTarget' {..} =
    Prelude.rnf forceUnsubscribeAll
      `Prelude.seq` Prelude.rnf targetAddress

instance Core.ToHeaders DeleteTarget where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteTarget where
  toJSON DeleteTarget' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ForceUnsubscribeAll" Core..=)
              Prelude.<$> forceUnsubscribeAll,
            Prelude.Just
              ("TargetAddress" Core..= targetAddress)
          ]
      )

instance Core.ToPath DeleteTarget where
  toPath = Prelude.const "/deleteTarget"

instance Core.ToQuery DeleteTarget where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTargetResponse' smart constructor.
data DeleteTargetResponse = DeleteTargetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTargetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTargetResponse_httpStatus' - The response's http status code.
newDeleteTargetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTargetResponse
newDeleteTargetResponse pHttpStatus_ =
  DeleteTargetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteTargetResponse_httpStatus :: Lens.Lens' DeleteTargetResponse Prelude.Int
deleteTargetResponse_httpStatus = Lens.lens (\DeleteTargetResponse' {httpStatus} -> httpStatus) (\s@DeleteTargetResponse' {} a -> s {httpStatus = a} :: DeleteTargetResponse)

instance Prelude.NFData DeleteTargetResponse where
  rnf DeleteTargetResponse' {..} =
    Prelude.rnf httpStatus
