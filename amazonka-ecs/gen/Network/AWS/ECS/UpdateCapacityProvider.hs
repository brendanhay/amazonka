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

import qualified Network.AWS.Core as Core
import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateCapacityProvider' smart constructor.
data UpdateCapacityProvider = UpdateCapacityProvider'
  { -- | The name of the capacity provider to update.
    name :: Core.Text,
    -- | An object representing the parameters to update for the Auto Scaling
    -- group capacity provider.
    autoScalingGroupProvider :: AutoScalingGroupProviderUpdate
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
updateCapacityProvider_name :: Lens.Lens' UpdateCapacityProvider Core.Text
updateCapacityProvider_name = Lens.lens (\UpdateCapacityProvider' {name} -> name) (\s@UpdateCapacityProvider' {} a -> s {name = a} :: UpdateCapacityProvider)

-- | An object representing the parameters to update for the Auto Scaling
-- group capacity provider.
updateCapacityProvider_autoScalingGroupProvider :: Lens.Lens' UpdateCapacityProvider AutoScalingGroupProviderUpdate
updateCapacityProvider_autoScalingGroupProvider = Lens.lens (\UpdateCapacityProvider' {autoScalingGroupProvider} -> autoScalingGroupProvider) (\s@UpdateCapacityProvider' {} a -> s {autoScalingGroupProvider = a} :: UpdateCapacityProvider)

instance Core.AWSRequest UpdateCapacityProvider where
  type
    AWSResponse UpdateCapacityProvider =
      UpdateCapacityProviderResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCapacityProviderResponse'
            Core.<$> (x Core..?> "capacityProvider")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateCapacityProvider

instance Core.NFData UpdateCapacityProvider

instance Core.ToHeaders UpdateCapacityProvider where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerServiceV20141113.UpdateCapacityProvider" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateCapacityProvider where
  toJSON UpdateCapacityProvider' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just
              ( "autoScalingGroupProvider"
                  Core..= autoScalingGroupProvider
              )
          ]
      )

instance Core.ToPath UpdateCapacityProvider where
  toPath = Core.const "/"

instance Core.ToQuery UpdateCapacityProvider where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCapacityProviderResponse' smart constructor.
data UpdateCapacityProviderResponse = UpdateCapacityProviderResponse'
  { capacityProvider :: Core.Maybe CapacityProvider,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateCapacityProviderResponse
newUpdateCapacityProviderResponse pHttpStatus_ =
  UpdateCapacityProviderResponse'
    { capacityProvider =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateCapacityProviderResponse_capacityProvider :: Lens.Lens' UpdateCapacityProviderResponse (Core.Maybe CapacityProvider)
updateCapacityProviderResponse_capacityProvider = Lens.lens (\UpdateCapacityProviderResponse' {capacityProvider} -> capacityProvider) (\s@UpdateCapacityProviderResponse' {} a -> s {capacityProvider = a} :: UpdateCapacityProviderResponse)

-- | The response's http status code.
updateCapacityProviderResponse_httpStatus :: Lens.Lens' UpdateCapacityProviderResponse Core.Int
updateCapacityProviderResponse_httpStatus = Lens.lens (\UpdateCapacityProviderResponse' {httpStatus} -> httpStatus) (\s@UpdateCapacityProviderResponse' {} a -> s {httpStatus = a} :: UpdateCapacityProviderResponse)

instance Core.NFData UpdateCapacityProviderResponse
