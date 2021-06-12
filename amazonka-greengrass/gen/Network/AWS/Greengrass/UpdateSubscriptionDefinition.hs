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
-- Module      : Network.AWS.Greengrass.UpdateSubscriptionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subscription definition.
module Network.AWS.Greengrass.UpdateSubscriptionDefinition
  ( -- * Creating a Request
    UpdateSubscriptionDefinition (..),
    newUpdateSubscriptionDefinition,

    -- * Request Lenses
    updateSubscriptionDefinition_name,
    updateSubscriptionDefinition_subscriptionDefinitionId,

    -- * Destructuring the Response
    UpdateSubscriptionDefinitionResponse (..),
    newUpdateSubscriptionDefinitionResponse,

    -- * Response Lenses
    updateSubscriptionDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSubscriptionDefinition' smart constructor.
data UpdateSubscriptionDefinition = UpdateSubscriptionDefinition'
  { -- | The name of the definition.
    name :: Core.Maybe Core.Text,
    -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSubscriptionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateSubscriptionDefinition_name' - The name of the definition.
--
-- 'subscriptionDefinitionId', 'updateSubscriptionDefinition_subscriptionDefinitionId' - The ID of the subscription definition.
newUpdateSubscriptionDefinition ::
  -- | 'subscriptionDefinitionId'
  Core.Text ->
  UpdateSubscriptionDefinition
newUpdateSubscriptionDefinition
  pSubscriptionDefinitionId_ =
    UpdateSubscriptionDefinition'
      { name = Core.Nothing,
        subscriptionDefinitionId =
          pSubscriptionDefinitionId_
      }

-- | The name of the definition.
updateSubscriptionDefinition_name :: Lens.Lens' UpdateSubscriptionDefinition (Core.Maybe Core.Text)
updateSubscriptionDefinition_name = Lens.lens (\UpdateSubscriptionDefinition' {name} -> name) (\s@UpdateSubscriptionDefinition' {} a -> s {name = a} :: UpdateSubscriptionDefinition)

-- | The ID of the subscription definition.
updateSubscriptionDefinition_subscriptionDefinitionId :: Lens.Lens' UpdateSubscriptionDefinition Core.Text
updateSubscriptionDefinition_subscriptionDefinitionId = Lens.lens (\UpdateSubscriptionDefinition' {subscriptionDefinitionId} -> subscriptionDefinitionId) (\s@UpdateSubscriptionDefinition' {} a -> s {subscriptionDefinitionId = a} :: UpdateSubscriptionDefinition)

instance Core.AWSRequest UpdateSubscriptionDefinition where
  type
    AWSResponse UpdateSubscriptionDefinition =
      UpdateSubscriptionDefinitionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSubscriptionDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateSubscriptionDefinition

instance Core.NFData UpdateSubscriptionDefinition

instance Core.ToHeaders UpdateSubscriptionDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateSubscriptionDefinition where
  toJSON UpdateSubscriptionDefinition' {..} =
    Core.object
      (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.ToPath UpdateSubscriptionDefinition where
  toPath UpdateSubscriptionDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/subscriptions/",
        Core.toBS subscriptionDefinitionId
      ]

instance Core.ToQuery UpdateSubscriptionDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateSubscriptionDefinitionResponse' smart constructor.
data UpdateSubscriptionDefinitionResponse = UpdateSubscriptionDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateSubscriptionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSubscriptionDefinitionResponse_httpStatus' - The response's http status code.
newUpdateSubscriptionDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateSubscriptionDefinitionResponse
newUpdateSubscriptionDefinitionResponse pHttpStatus_ =
  UpdateSubscriptionDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateSubscriptionDefinitionResponse_httpStatus :: Lens.Lens' UpdateSubscriptionDefinitionResponse Core.Int
updateSubscriptionDefinitionResponse_httpStatus = Lens.lens (\UpdateSubscriptionDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriptionDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateSubscriptionDefinitionResponse)

instance
  Core.NFData
    UpdateSubscriptionDefinitionResponse
