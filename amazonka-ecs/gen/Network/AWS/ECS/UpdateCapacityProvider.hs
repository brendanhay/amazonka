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
-- Module      : Network.AWS.ECS.UpdateCapacityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the parameters for a capacity provider.
module Network.AWS.ECS.UpdateCapacityProvider
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

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCapacityProvider' smart constructor.
data UpdateCapacityProvider = UpdateCapacityProvider'
  { -- | The name of the capacity provider to update.
    name :: Prelude.Text,
    -- | An object representing the parameters to update for the Auto Scaling
    -- group capacity provider.
    autoScalingGroupProvider :: AutoScalingGroupProviderUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'autoScalingGroupProvider', 'updateCapacityProvider_autoScalingGroupProvider' - An object representing the parameters to update for the Auto Scaling
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

-- | An object representing the parameters to update for the Auto Scaling
-- group capacity provider.
updateCapacityProvider_autoScalingGroupProvider :: Lens.Lens' UpdateCapacityProvider AutoScalingGroupProviderUpdate
updateCapacityProvider_autoScalingGroupProvider = Lens.lens (\UpdateCapacityProvider' {autoScalingGroupProvider} -> autoScalingGroupProvider) (\s@UpdateCapacityProvider' {} a -> s {autoScalingGroupProvider = a} :: UpdateCapacityProvider)

instance Prelude.AWSRequest UpdateCapacityProvider where
  type
    Rs UpdateCapacityProvider =
      UpdateCapacityProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCapacityProviderResponse'
            Prelude.<$> (x Prelude..?> "capacityProvider")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateCapacityProvider

instance Prelude.NFData UpdateCapacityProvider

instance Prelude.ToHeaders UpdateCapacityProvider where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonEC2ContainerServiceV20141113.UpdateCapacityProvider" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateCapacityProvider where
  toJSON UpdateCapacityProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Prelude..= name),
            Prelude.Just
              ( "autoScalingGroupProvider"
                  Prelude..= autoScalingGroupProvider
              )
          ]
      )

instance Prelude.ToPath UpdateCapacityProvider where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateCapacityProvider where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateCapacityProviderResponse' smart constructor.
data UpdateCapacityProviderResponse = UpdateCapacityProviderResponse'
  { capacityProvider :: Prelude.Maybe CapacityProvider,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateCapacityProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityProvider', 'updateCapacityProviderResponse_capacityProvider' - Undocumented member.
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

-- | Undocumented member.
updateCapacityProviderResponse_capacityProvider :: Lens.Lens' UpdateCapacityProviderResponse (Prelude.Maybe CapacityProvider)
updateCapacityProviderResponse_capacityProvider = Lens.lens (\UpdateCapacityProviderResponse' {capacityProvider} -> capacityProvider) (\s@UpdateCapacityProviderResponse' {} a -> s {capacityProvider = a} :: UpdateCapacityProviderResponse)

-- | The response's http status code.
updateCapacityProviderResponse_httpStatus :: Lens.Lens' UpdateCapacityProviderResponse Prelude.Int
updateCapacityProviderResponse_httpStatus = Lens.lens (\UpdateCapacityProviderResponse' {httpStatus} -> httpStatus) (\s@UpdateCapacityProviderResponse' {} a -> s {httpStatus = a} :: UpdateCapacityProviderResponse)

instance
  Prelude.NFData
    UpdateCapacityProviderResponse
