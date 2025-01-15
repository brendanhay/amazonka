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
-- Module      : Amazonka.ECS.UpdateCapacityProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters for a capacity provider.
module Amazonka.ECS.UpdateCapacityProvider
  ( -- * Creating a Request
    UpdateCapacityProvider (..),
    newUpdateCapacityProvider,

    -- * Request Lenses
    updateCapacityProvider_name,
    updateCapacityProvider_autoScalingGroupProvider,

    -- * Destructuring the Response
    UpdateCapacityProviderResponse (..),
    newUpdateCapacityProviderResponse,

    -- * Response Lenses
    updateCapacityProviderResponse_capacityProvider,
    updateCapacityProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateCapacityProvider' smart constructor.
data UpdateCapacityProvider = UpdateCapacityProvider'
  { -- | The name of the capacity provider to update.
    name :: Prelude.Text,
    -- | An object that represent the parameters to update for the Auto Scaling
    -- group capacity provider.
    autoScalingGroupProvider :: AutoScalingGroupProviderUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCapacityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateCapacityProvider_name' - The name of the capacity provider to update.
--
-- 'autoScalingGroupProvider', 'updateCapacityProvider_autoScalingGroupProvider' - An object that represent the parameters to update for the Auto Scaling
-- group capacity provider.
newUpdateCapacityProvider ::
  -- | 'name'
  Prelude.Text ->
  -- | 'autoScalingGroupProvider'
  AutoScalingGroupProviderUpdate ->
  UpdateCapacityProvider
newUpdateCapacityProvider
  pName_
  pAutoScalingGroupProvider_ =
    UpdateCapacityProvider'
      { name = pName_,
        autoScalingGroupProvider =
          pAutoScalingGroupProvider_
      }

-- | The name of the capacity provider to update.
updateCapacityProvider_name :: Lens.Lens' UpdateCapacityProvider Prelude.Text
updateCapacityProvider_name = Lens.lens (\UpdateCapacityProvider' {name} -> name) (\s@UpdateCapacityProvider' {} a -> s {name = a} :: UpdateCapacityProvider)

-- | An object that represent the parameters to update for the Auto Scaling
-- group capacity provider.
updateCapacityProvider_autoScalingGroupProvider :: Lens.Lens' UpdateCapacityProvider AutoScalingGroupProviderUpdate
updateCapacityProvider_autoScalingGroupProvider = Lens.lens (\UpdateCapacityProvider' {autoScalingGroupProvider} -> autoScalingGroupProvider) (\s@UpdateCapacityProvider' {} a -> s {autoScalingGroupProvider = a} :: UpdateCapacityProvider)

instance Core.AWSRequest UpdateCapacityProvider where
  type
    AWSResponse UpdateCapacityProvider =
      UpdateCapacityProviderResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCapacityProviderResponse'
            Prelude.<$> (x Data..?> "capacityProvider")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCapacityProvider where
  hashWithSalt _salt UpdateCapacityProvider' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` autoScalingGroupProvider

instance Prelude.NFData UpdateCapacityProvider where
  rnf UpdateCapacityProvider' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf autoScalingGroupProvider

instance Data.ToHeaders UpdateCapacityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonEC2ContainerServiceV20141113.UpdateCapacityProvider" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateCapacityProvider where
  toJSON UpdateCapacityProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just
              ( "autoScalingGroupProvider"
                  Data..= autoScalingGroupProvider
              )
          ]
      )

instance Data.ToPath UpdateCapacityProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateCapacityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCapacityProviderResponse' smart constructor.
data UpdateCapacityProviderResponse = UpdateCapacityProviderResponse'
  { -- | Details about the capacity provider.
    capacityProvider :: Prelude.Maybe CapacityProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateCapacityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProvider', 'updateCapacityProviderResponse_capacityProvider' - Details about the capacity provider.
--
-- 'httpStatus', 'updateCapacityProviderResponse_httpStatus' - The response's http status code.
newUpdateCapacityProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateCapacityProviderResponse
newUpdateCapacityProviderResponse pHttpStatus_ =
  UpdateCapacityProviderResponse'
    { capacityProvider =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the capacity provider.
updateCapacityProviderResponse_capacityProvider :: Lens.Lens' UpdateCapacityProviderResponse (Prelude.Maybe CapacityProvider)
updateCapacityProviderResponse_capacityProvider = Lens.lens (\UpdateCapacityProviderResponse' {capacityProvider} -> capacityProvider) (\s@UpdateCapacityProviderResponse' {} a -> s {capacityProvider = a} :: UpdateCapacityProviderResponse)

-- | The response's http status code.
updateCapacityProviderResponse_httpStatus :: Lens.Lens' UpdateCapacityProviderResponse Prelude.Int
updateCapacityProviderResponse_httpStatus = Lens.lens (\UpdateCapacityProviderResponse' {httpStatus} -> httpStatus) (\s@UpdateCapacityProviderResponse' {} a -> s {httpStatus = a} :: UpdateCapacityProviderResponse)

instance
  Prelude.NFData
    UpdateCapacityProviderResponse
  where
  rnf UpdateCapacityProviderResponse' {..} =
    Prelude.rnf capacityProvider `Prelude.seq`
      Prelude.rnf httpStatus
