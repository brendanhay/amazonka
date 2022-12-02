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
-- Module      : Amazonka.Connect.DeleteContactFlowModule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified flow module.
module Amazonka.Connect.DeleteContactFlowModule
  ( -- * Creating a Request
    DeleteContactFlowModule (..),
    newDeleteContactFlowModule,

    -- * Request Lenses
    deleteContactFlowModule_instanceId,
    deleteContactFlowModule_contactFlowModuleId,

    -- * Destructuring the Response
    DeleteContactFlowModuleResponse (..),
    newDeleteContactFlowModuleResponse,

    -- * Response Lenses
    deleteContactFlowModuleResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteContactFlowModule' smart constructor.
data DeleteContactFlowModule = DeleteContactFlowModule'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the flow module.
    contactFlowModuleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactFlowModule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteContactFlowModule_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactFlowModuleId', 'deleteContactFlowModule_contactFlowModuleId' - The identifier of the flow module.
newDeleteContactFlowModule ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowModuleId'
  Prelude.Text ->
  DeleteContactFlowModule
newDeleteContactFlowModule
  pInstanceId_
  pContactFlowModuleId_ =
    DeleteContactFlowModule'
      { instanceId = pInstanceId_,
        contactFlowModuleId = pContactFlowModuleId_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteContactFlowModule_instanceId :: Lens.Lens' DeleteContactFlowModule Prelude.Text
deleteContactFlowModule_instanceId = Lens.lens (\DeleteContactFlowModule' {instanceId} -> instanceId) (\s@DeleteContactFlowModule' {} a -> s {instanceId = a} :: DeleteContactFlowModule)

-- | The identifier of the flow module.
deleteContactFlowModule_contactFlowModuleId :: Lens.Lens' DeleteContactFlowModule Prelude.Text
deleteContactFlowModule_contactFlowModuleId = Lens.lens (\DeleteContactFlowModule' {contactFlowModuleId} -> contactFlowModuleId) (\s@DeleteContactFlowModule' {} a -> s {contactFlowModuleId = a} :: DeleteContactFlowModule)

instance Core.AWSRequest DeleteContactFlowModule where
  type
    AWSResponse DeleteContactFlowModule =
      DeleteContactFlowModuleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteContactFlowModuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteContactFlowModule where
  hashWithSalt _salt DeleteContactFlowModule' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactFlowModuleId

instance Prelude.NFData DeleteContactFlowModule where
  rnf DeleteContactFlowModule' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactFlowModuleId

instance Data.ToHeaders DeleteContactFlowModule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteContactFlowModule where
  toPath DeleteContactFlowModule' {..} =
    Prelude.mconcat
      [ "/contact-flow-modules/",
        Data.toBS instanceId,
        "/",
        Data.toBS contactFlowModuleId
      ]

instance Data.ToQuery DeleteContactFlowModule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContactFlowModuleResponse' smart constructor.
data DeleteContactFlowModuleResponse = DeleteContactFlowModuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactFlowModuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteContactFlowModuleResponse_httpStatus' - The response's http status code.
newDeleteContactFlowModuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteContactFlowModuleResponse
newDeleteContactFlowModuleResponse pHttpStatus_ =
  DeleteContactFlowModuleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteContactFlowModuleResponse_httpStatus :: Lens.Lens' DeleteContactFlowModuleResponse Prelude.Int
deleteContactFlowModuleResponse_httpStatus = Lens.lens (\DeleteContactFlowModuleResponse' {httpStatus} -> httpStatus) (\s@DeleteContactFlowModuleResponse' {} a -> s {httpStatus = a} :: DeleteContactFlowModuleResponse)

instance
  Prelude.NFData
    DeleteContactFlowModuleResponse
  where
  rnf DeleteContactFlowModuleResponse' {..} =
    Prelude.rnf httpStatus
