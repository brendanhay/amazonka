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
-- Module      : Amazonka.Connect.DeleteContactFlow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a flow for the specified Amazon Connect instance.
module Amazonka.Connect.DeleteContactFlow
  ( -- * Creating a Request
    DeleteContactFlow (..),
    newDeleteContactFlow,

    -- * Request Lenses
    deleteContactFlow_instanceId,
    deleteContactFlow_contactFlowId,

    -- * Destructuring the Response
    DeleteContactFlowResponse (..),
    newDeleteContactFlowResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteContactFlow' smart constructor.
data DeleteContactFlow = DeleteContactFlow'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the flow.
    contactFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactFlow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteContactFlow_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactFlowId', 'deleteContactFlow_contactFlowId' - The identifier of the flow.
newDeleteContactFlow ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  DeleteContactFlow
newDeleteContactFlow pInstanceId_ pContactFlowId_ =
  DeleteContactFlow'
    { instanceId = pInstanceId_,
      contactFlowId = pContactFlowId_
    }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteContactFlow_instanceId :: Lens.Lens' DeleteContactFlow Prelude.Text
deleteContactFlow_instanceId = Lens.lens (\DeleteContactFlow' {instanceId} -> instanceId) (\s@DeleteContactFlow' {} a -> s {instanceId = a} :: DeleteContactFlow)

-- | The identifier of the flow.
deleteContactFlow_contactFlowId :: Lens.Lens' DeleteContactFlow Prelude.Text
deleteContactFlow_contactFlowId = Lens.lens (\DeleteContactFlow' {contactFlowId} -> contactFlowId) (\s@DeleteContactFlow' {} a -> s {contactFlowId = a} :: DeleteContactFlow)

instance Core.AWSRequest DeleteContactFlow where
  type
    AWSResponse DeleteContactFlow =
      DeleteContactFlowResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteContactFlowResponse'

instance Prelude.Hashable DeleteContactFlow where
  hashWithSalt _salt DeleteContactFlow' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactFlowId

instance Prelude.NFData DeleteContactFlow where
  rnf DeleteContactFlow' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactFlowId

instance Data.ToHeaders DeleteContactFlow where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteContactFlow where
  toPath DeleteContactFlow' {..} =
    Prelude.mconcat
      [ "/contact-flows/",
        Data.toBS instanceId,
        "/",
        Data.toBS contactFlowId
      ]

instance Data.ToQuery DeleteContactFlow where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteContactFlowResponse' smart constructor.
data DeleteContactFlowResponse = DeleteContactFlowResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteContactFlowResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteContactFlowResponse ::
  DeleteContactFlowResponse
newDeleteContactFlowResponse =
  DeleteContactFlowResponse'

instance Prelude.NFData DeleteContactFlowResponse where
  rnf _ = ()
