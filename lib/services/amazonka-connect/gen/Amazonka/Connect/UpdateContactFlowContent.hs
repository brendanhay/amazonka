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
-- Module      : Amazonka.Connect.UpdateContactFlowContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified flow.
--
-- You can also create and update flows using the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
module Amazonka.Connect.UpdateContactFlowContent
  ( -- * Creating a Request
    UpdateContactFlowContent (..),
    newUpdateContactFlowContent,

    -- * Request Lenses
    updateContactFlowContent_instanceId,
    updateContactFlowContent_contactFlowId,
    updateContactFlowContent_content,

    -- * Destructuring the Response
    UpdateContactFlowContentResponse (..),
    newUpdateContactFlowContentResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateContactFlowContent' smart constructor.
data UpdateContactFlowContent = UpdateContactFlowContent'
  { -- | The identifier of the Amazon Connect instance.
    instanceId :: Prelude.Text,
    -- | The identifier of the flow.
    contactFlowId :: Prelude.Text,
    -- | The JSON string that represents flow\'s content. For an example, see
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language-example.html Example contact flow in Amazon Connect Flow language>
    -- in the /Amazon Connect Administrator Guide/.
    content :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactFlowContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'updateContactFlowContent_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'contactFlowId', 'updateContactFlowContent_contactFlowId' - The identifier of the flow.
--
-- 'content', 'updateContactFlowContent_content' - The JSON string that represents flow\'s content. For an example, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language-example.html Example contact flow in Amazon Connect Flow language>
-- in the /Amazon Connect Administrator Guide/.
newUpdateContactFlowContent ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  -- | 'content'
  Prelude.Text ->
  UpdateContactFlowContent
newUpdateContactFlowContent
  pInstanceId_
  pContactFlowId_
  pContent_ =
    UpdateContactFlowContent'
      { instanceId =
          pInstanceId_,
        contactFlowId = pContactFlowId_,
        content = pContent_
      }

-- | The identifier of the Amazon Connect instance.
updateContactFlowContent_instanceId :: Lens.Lens' UpdateContactFlowContent Prelude.Text
updateContactFlowContent_instanceId = Lens.lens (\UpdateContactFlowContent' {instanceId} -> instanceId) (\s@UpdateContactFlowContent' {} a -> s {instanceId = a} :: UpdateContactFlowContent)

-- | The identifier of the flow.
updateContactFlowContent_contactFlowId :: Lens.Lens' UpdateContactFlowContent Prelude.Text
updateContactFlowContent_contactFlowId = Lens.lens (\UpdateContactFlowContent' {contactFlowId} -> contactFlowId) (\s@UpdateContactFlowContent' {} a -> s {contactFlowId = a} :: UpdateContactFlowContent)

-- | The JSON string that represents flow\'s content. For an example, see
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language-example.html Example contact flow in Amazon Connect Flow language>
-- in the /Amazon Connect Administrator Guide/.
updateContactFlowContent_content :: Lens.Lens' UpdateContactFlowContent Prelude.Text
updateContactFlowContent_content = Lens.lens (\UpdateContactFlowContent' {content} -> content) (\s@UpdateContactFlowContent' {} a -> s {content = a} :: UpdateContactFlowContent)

instance Core.AWSRequest UpdateContactFlowContent where
  type
    AWSResponse UpdateContactFlowContent =
      UpdateContactFlowContentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      UpdateContactFlowContentResponse'

instance Prelude.Hashable UpdateContactFlowContent where
  hashWithSalt _salt UpdateContactFlowContent' {..} =
    _salt `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` contactFlowId
      `Prelude.hashWithSalt` content

instance Prelude.NFData UpdateContactFlowContent where
  rnf UpdateContactFlowContent' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf contactFlowId
      `Prelude.seq` Prelude.rnf content

instance Core.ToHeaders UpdateContactFlowContent where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateContactFlowContent where
  toJSON UpdateContactFlowContent' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Content" Core..= content)]
      )

instance Core.ToPath UpdateContactFlowContent where
  toPath UpdateContactFlowContent' {..} =
    Prelude.mconcat
      [ "/contact-flows/",
        Core.toBS instanceId,
        "/",
        Core.toBS contactFlowId,
        "/content"
      ]

instance Core.ToQuery UpdateContactFlowContent where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateContactFlowContentResponse' smart constructor.
data UpdateContactFlowContentResponse = UpdateContactFlowContentResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateContactFlowContentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateContactFlowContentResponse ::
  UpdateContactFlowContentResponse
newUpdateContactFlowContentResponse =
  UpdateContactFlowContentResponse'

instance
  Prelude.NFData
    UpdateContactFlowContentResponse
  where
  rnf _ = ()
