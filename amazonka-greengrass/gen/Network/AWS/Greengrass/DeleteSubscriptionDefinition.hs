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
-- Module      : Network.AWS.Greengrass.DeleteSubscriptionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a subscription definition.
module Network.AWS.Greengrass.DeleteSubscriptionDefinition
  ( -- * Creating a Request
    DeleteSubscriptionDefinition (..),
    newDeleteSubscriptionDefinition,

    -- * Request Lenses
    deleteSubscriptionDefinition_subscriptionDefinitionId,

    -- * Destructuring the Response
    DeleteSubscriptionDefinitionResponse (..),
    newDeleteSubscriptionDefinitionResponse,

    -- * Response Lenses
    deleteSubscriptionDefinitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteSubscriptionDefinition' smart constructor.
data DeleteSubscriptionDefinition = DeleteSubscriptionDefinition'
  { -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSubscriptionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subscriptionDefinitionId', 'deleteSubscriptionDefinition_subscriptionDefinitionId' - The ID of the subscription definition.
newDeleteSubscriptionDefinition ::
  -- | 'subscriptionDefinitionId'
  Core.Text ->
  DeleteSubscriptionDefinition
newDeleteSubscriptionDefinition
  pSubscriptionDefinitionId_ =
    DeleteSubscriptionDefinition'
      { subscriptionDefinitionId =
          pSubscriptionDefinitionId_
      }

-- | The ID of the subscription definition.
deleteSubscriptionDefinition_subscriptionDefinitionId :: Lens.Lens' DeleteSubscriptionDefinition Core.Text
deleteSubscriptionDefinition_subscriptionDefinitionId = Lens.lens (\DeleteSubscriptionDefinition' {subscriptionDefinitionId} -> subscriptionDefinitionId) (\s@DeleteSubscriptionDefinition' {} a -> s {subscriptionDefinitionId = a} :: DeleteSubscriptionDefinition)

instance Core.AWSRequest DeleteSubscriptionDefinition where
  type
    AWSResponse DeleteSubscriptionDefinition =
      DeleteSubscriptionDefinitionResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSubscriptionDefinitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteSubscriptionDefinition

instance Core.NFData DeleteSubscriptionDefinition

instance Core.ToHeaders DeleteSubscriptionDefinition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteSubscriptionDefinition where
  toPath DeleteSubscriptionDefinition' {..} =
    Core.mconcat
      [ "/greengrass/definition/subscriptions/",
        Core.toBS subscriptionDefinitionId
      ]

instance Core.ToQuery DeleteSubscriptionDefinition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteSubscriptionDefinitionResponse' smart constructor.
data DeleteSubscriptionDefinitionResponse = DeleteSubscriptionDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteSubscriptionDefinitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSubscriptionDefinitionResponse_httpStatus' - The response's http status code.
newDeleteSubscriptionDefinitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteSubscriptionDefinitionResponse
newDeleteSubscriptionDefinitionResponse pHttpStatus_ =
  DeleteSubscriptionDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSubscriptionDefinitionResponse_httpStatus :: Lens.Lens' DeleteSubscriptionDefinitionResponse Core.Int
deleteSubscriptionDefinitionResponse_httpStatus = Lens.lens (\DeleteSubscriptionDefinitionResponse' {httpStatus} -> httpStatus) (\s@DeleteSubscriptionDefinitionResponse' {} a -> s {httpStatus = a} :: DeleteSubscriptionDefinitionResponse)

instance
  Core.NFData
    DeleteSubscriptionDefinitionResponse
