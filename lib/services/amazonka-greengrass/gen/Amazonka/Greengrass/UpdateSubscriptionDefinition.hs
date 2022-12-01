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
-- Module      : Amazonka.Greengrass.UpdateSubscriptionDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a subscription definition.
module Amazonka.Greengrass.UpdateSubscriptionDefinition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSubscriptionDefinition' smart constructor.
data UpdateSubscriptionDefinition = UpdateSubscriptionDefinition'
  { -- | The name of the definition.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subscription definition.
    subscriptionDefinitionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest UpdateSubscriptionDefinition where
  type
    AWSResponse UpdateSubscriptionDefinition =
      UpdateSubscriptionDefinitionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSubscriptionDefinitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSubscriptionDefinition
  where
  hashWithSalt _salt UpdateSubscriptionDefinition' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` subscriptionDefinitionId

instance Prelude.NFData UpdateSubscriptionDefinition where
  rnf UpdateSubscriptionDefinition' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf subscriptionDefinitionId

instance Core.ToHeaders UpdateSubscriptionDefinition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSubscriptionDefinition where
  toJSON UpdateSubscriptionDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Name" Core..=) Prelude.<$> name]
      )

instance Core.ToPath UpdateSubscriptionDefinition where
  toPath UpdateSubscriptionDefinition' {..} =
    Prelude.mconcat
      [ "/greengrass/definition/subscriptions/",
        Core.toBS subscriptionDefinitionId
      ]

instance Core.ToQuery UpdateSubscriptionDefinition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSubscriptionDefinitionResponse' smart constructor.
data UpdateSubscriptionDefinitionResponse = UpdateSubscriptionDefinitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf UpdateSubscriptionDefinitionResponse' {..} =
    Prelude.rnf httpStatus
