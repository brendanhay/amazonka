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
-- Module      : Network.AWS.Connect.UpdateContactFlowName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The name of the contact flow.
--
-- You can also create and update contact flows using the
-- <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language>.
module Network.AWS.Connect.UpdateContactFlowName
  ( -- * Creating a Request
    UpdateContactFlowName (..),
    newUpdateContactFlowName,

    -- * Request Lenses
    updateContactFlowName_name,
    updateContactFlowName_description,
    updateContactFlowName_instanceId,
    updateContactFlowName_contactFlowId,

    -- * Destructuring the Response
    UpdateContactFlowNameResponse (..),
    newUpdateContactFlowNameResponse,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateContactFlowName' smart constructor.
data UpdateContactFlowName = UpdateContactFlowName'
  { -- | The name of the contact flow.
    name :: Core.Maybe Core.Text,
    -- | The description of the contact flow.
    description :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text,
    -- | The identifier of the contact flow.
    contactFlowId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateContactFlowName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateContactFlowName_name' - The name of the contact flow.
--
-- 'description', 'updateContactFlowName_description' - The description of the contact flow.
--
-- 'instanceId', 'updateContactFlowName_instanceId' - The identifier of the Amazon Connect instance.
--
-- 'contactFlowId', 'updateContactFlowName_contactFlowId' - The identifier of the contact flow.
newUpdateContactFlowName ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'contactFlowId'
  Core.Text ->
  UpdateContactFlowName
newUpdateContactFlowName pInstanceId_ pContactFlowId_ =
  UpdateContactFlowName'
    { name = Core.Nothing,
      description = Core.Nothing,
      instanceId = pInstanceId_,
      contactFlowId = pContactFlowId_
    }

-- | The name of the contact flow.
updateContactFlowName_name :: Lens.Lens' UpdateContactFlowName (Core.Maybe Core.Text)
updateContactFlowName_name = Lens.lens (\UpdateContactFlowName' {name} -> name) (\s@UpdateContactFlowName' {} a -> s {name = a} :: UpdateContactFlowName)

-- | The description of the contact flow.
updateContactFlowName_description :: Lens.Lens' UpdateContactFlowName (Core.Maybe Core.Text)
updateContactFlowName_description = Lens.lens (\UpdateContactFlowName' {description} -> description) (\s@UpdateContactFlowName' {} a -> s {description = a} :: UpdateContactFlowName)

-- | The identifier of the Amazon Connect instance.
updateContactFlowName_instanceId :: Lens.Lens' UpdateContactFlowName Core.Text
updateContactFlowName_instanceId = Lens.lens (\UpdateContactFlowName' {instanceId} -> instanceId) (\s@UpdateContactFlowName' {} a -> s {instanceId = a} :: UpdateContactFlowName)

-- | The identifier of the contact flow.
updateContactFlowName_contactFlowId :: Lens.Lens' UpdateContactFlowName Core.Text
updateContactFlowName_contactFlowId = Lens.lens (\UpdateContactFlowName' {contactFlowId} -> contactFlowId) (\s@UpdateContactFlowName' {} a -> s {contactFlowId = a} :: UpdateContactFlowName)

instance Core.AWSRequest UpdateContactFlowName where
  type
    AWSResponse UpdateContactFlowName =
      UpdateContactFlowNameResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull UpdateContactFlowNameResponse'

instance Core.Hashable UpdateContactFlowName

instance Core.NFData UpdateContactFlowName

instance Core.ToHeaders UpdateContactFlowName where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateContactFlowName where
  toJSON UpdateContactFlowName' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Description" Core..=) Core.<$> description
          ]
      )

instance Core.ToPath UpdateContactFlowName where
  toPath UpdateContactFlowName' {..} =
    Core.mconcat
      [ "/contact-flows/",
        Core.toBS instanceId,
        "/",
        Core.toBS contactFlowId,
        "/name"
      ]

instance Core.ToQuery UpdateContactFlowName where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateContactFlowNameResponse' smart constructor.
data UpdateContactFlowNameResponse = UpdateContactFlowNameResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateContactFlowNameResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateContactFlowNameResponse ::
  UpdateContactFlowNameResponse
newUpdateContactFlowNameResponse =
  UpdateContactFlowNameResponse'

instance Core.NFData UpdateContactFlowNameResponse
