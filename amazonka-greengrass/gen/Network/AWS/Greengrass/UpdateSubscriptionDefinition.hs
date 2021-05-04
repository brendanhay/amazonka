{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateSubscriptionDefinition' smart constructor.
data UpdateSubscriptionDefinition = UpdateSubscriptionDefinition'
  { -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateSubscriptionDefinition
newUpdateSubscriptionDefinition
  pSubscriptionDefinitionId_ =
    UpdateSubscriptionDefinition'
      { name =
          Prelude.Nothing,
        subscriptionDefinitionId =
          pSubscriptionDefinitionId_
      }

-- | The name of the definition.
updateSubscriptionDefinition_name :: Lens.Lens' UpdateSubscriptionDefinition (Prelude.Maybe Prelude.Text)
updateSubscriptionDefinition_name = Lens.lens (\UpdateSubscriptionDefinition' {name} -> name) (\s@UpdateSubscriptionDefinition' {} a -> s {name = a} :: UpdateSubscriptionDefinition)

-- | The ID of the subscription definition.
updateSubscriptionDefinition_subscriptionDefinitionId :: Lens.Lens' UpdateSubscriptionDefinition Prelude.Text
updateSubscriptionDefinition_subscriptionDefinitionId = Lens.lens (\UpdateSubscriptionDefinition' {subscriptionDefinitionId} -> subscriptionDefinitionId) (\s@UpdateSubscriptionDefinition' {} a -> s {subscriptionDefinitionId = a} :: UpdateSubscriptionDefinition)

instance
  Prelude.AWSRequest
    UpdateSubscriptionDefinition
  where
  type
    Rs UpdateSubscriptionDefinition =
      UpdateSubscriptionDefinitionResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSubscriptionDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSubscriptionDefinition

instance Prelude.NFData UpdateSubscriptionDefinition

instance
  Prelude.ToHeaders
    UpdateSubscriptionDefinition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateSubscriptionDefinition where
  toJSON UpdateSubscriptionDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Name" Prelude..=) Prelude.<$> name]
      )

instance Prelude.ToPath UpdateSubscriptionDefinition where
  toPath UpdateSubscriptionDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Prelude.toBS subscriptionDefinitionId
      ]

instance Prelude.ToQuery UpdateSubscriptionDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSubscriptionDefinitionResponse' smart constructor.
data UpdateSubscriptionDefinitionResponse = UpdateSubscriptionDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateSubscriptionDefinitionResponse
newUpdateSubscriptionDefinitionResponse pHttpStatus_ =
  UpdateSubscriptionDefinitionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateSubscriptionDefinitionResponse_httpStatus :: Lens.Lens' UpdateSubscriptionDefinitionResponse Prelude.Int
updateSubscriptionDefinitionResponse_httpStatus = Lens.lens (\UpdateSubscriptionDefinitionResponse' {httpStatus} -> httpStatus) (\s@UpdateSubscriptionDefinitionResponse' {} a -> s {httpStatus = a} :: UpdateSubscriptionDefinitionResponse)

instance
  Prelude.NFData
    UpdateSubscriptionDefinitionResponse
