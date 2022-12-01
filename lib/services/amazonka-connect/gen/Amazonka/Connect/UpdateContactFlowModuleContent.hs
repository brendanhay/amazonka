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
-- Module      : Amazonka.Connect.UpdateContactFlowModuleContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates specified flow module for the specified Amazon Connect instance.
module Amazonka.Connect.UpdateContactFlowModuleContent
  ( -- * Creating a Request
    UpdateContactFlowModuleContent (..),
    newUpdateContactFlowModuleContent,

    -- * Request Lenses
    updateContactFlowModuleContent_instanceId,
    updateContactFlowModuleContent_contactFlowModuleId,
    updateContactFlowModuleContent_content,

    -- * Destructuring the Response
    UpdateContactFlowModuleContentResponse (..),
    newUpdateContactFlowModuleContentResponse,

    -- * Response Lenses
    updateContactFlowModuleContentResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContactFlowModuleContent' smart constructor.
data UpdateContactFlowModuleContent = UpdateContactFlowModuleContent'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the flow module.
    contactFlowModuleId :: Prelude.Text,
    -- | The content of the flow module.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactFlowModuleContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateContactFlowModuleContent_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'contactFlowModuleId', 'updateContactFlowModuleContent_contactFlowModuleId' - The identifier of the flow module.
--
-- 'content', 'updateContactFlowModuleContent_content' - The content of the flow module.
newUpdateContactFlowModuleContent ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowModuleId'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  UpdateContactFlowModuleContent
newUpdateContactFlowModuleContent
  pInstanceId_
  pContactFlowModuleId_
  pContent_ =
    UpdateContactFlowModuleContent'
      { instanceId =
          pInstanceId_,
        contactFlowModuleId = pContactFlowModuleId_,
        content = pContent_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
updateContactFlowModuleContent_instanceId :: Lens.Lens' UpdateContactFlowModuleContent Prelude.Text
updateContactFlowModuleContent_instanceId = Lens.lens (\UpdateContactFlowModuleContent' {instanceId} -> instanceId) (\s@UpdateContactFlowModuleContent' {} a -> s {instanceId = a} :: UpdateContactFlowModuleContent)

-- | The identifier of the flow module.
updateContactFlowModuleContent_contactFlowModuleId :: Lens.Lens' UpdateContactFlowModuleContent Prelude.Text
updateContactFlowModuleContent_contactFlowModuleId = Lens.lens (\UpdateContactFlowModuleContent' {contactFlowModuleId} -> contactFlowModuleId) (\s@UpdateContactFlowModuleContent' {} a -> s {contactFlowModuleId = a} :: UpdateContactFlowModuleContent)

-- | The content of the flow module.
updateContactFlowModuleContent_content :: Lens.Lens' UpdateContactFlowModuleContent Prelude.Text
updateContactFlowModuleContent_content = Lens.lens (\UpdateContactFlowModuleContent' {content} -> content) (\s@UpdateContactFlowModuleContent' {} a -> s {content = a} :: UpdateContactFlowModuleContent)

instance
  Core.AWSRequest
    UpdateContactFlowModuleContent
  where
  type
    AWSResponse UpdateContactFlowModuleContent =
      UpdateContactFlowModuleContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateContactFlowModuleContentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateContactFlowModuleContent
  where
  hashWithSalt
    _salt
    UpdateContactFlowModuleContent' {..} =
      _salt `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` contactFlowModuleId
        `Prelude.hashWithSalt` content

instance
  Prelude.NFData
    UpdateContactFlowModuleContent
  where
  rnf UpdateContactFlowModuleContent' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactFlowModuleId
      `Prelude.seq` Prelude.rnf content

instance
  Core.ToHeaders
    UpdateContactFlowModuleContent
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateContactFlowModuleContent where
  toJSON UpdateContactFlowModuleContent' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Content" Core..= content)]
      )

instance Core.ToPath UpdateContactFlowModuleContent where
  toPath UpdateContactFlowModuleContent' {..} =
    Prelude.mconcat
      [ "/contact-flow-modules/",
        Core.toBS instanceId,
        "/",
        Core.toBS contactFlowModuleId,
        "/content"
      ]

instance Core.ToQuery UpdateContactFlowModuleContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContactFlowModuleContentResponse' smart constructor.
data UpdateContactFlowModuleContentResponse = UpdateContactFlowModuleContentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactFlowModuleContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateContactFlowModuleContentResponse_httpStatus' - The response's http status code.
newUpdateContactFlowModuleContentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateContactFlowModuleContentResponse
newUpdateContactFlowModuleContentResponse
  pHttpStatus_ =
    UpdateContactFlowModuleContentResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateContactFlowModuleContentResponse_httpStatus :: Lens.Lens' UpdateContactFlowModuleContentResponse Prelude.Int
updateContactFlowModuleContentResponse_httpStatus = Lens.lens (\UpdateContactFlowModuleContentResponse' {httpStatus} -> httpStatus) (\s@UpdateContactFlowModuleContentResponse' {} a -> s {httpStatus = a} :: UpdateContactFlowModuleContentResponse)

instance
  Prelude.NFData
    UpdateContactFlowModuleContentResponse
  where
  rnf UpdateContactFlowModuleContentResponse' {..} =
    Prelude.rnf httpStatus
