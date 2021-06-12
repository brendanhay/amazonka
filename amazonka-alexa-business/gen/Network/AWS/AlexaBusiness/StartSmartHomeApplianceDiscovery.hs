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
-- Module      : Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the discovery of any smart home appliances associated with the
-- room.
module Network.AWS.AlexaBusiness.StartSmartHomeApplianceDiscovery
  ( -- * Creating a Request
    StartSmartHomeApplianceDiscovery (..),
    newStartSmartHomeApplianceDiscovery,

    -- * Request Lenses
    startSmartHomeApplianceDiscovery_roomArn,

    -- * Destructuring the Response
    StartSmartHomeApplianceDiscoveryResponse (..),
    newStartSmartHomeApplianceDiscoveryResponse,

    -- * Response Lenses
    startSmartHomeApplianceDiscoveryResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartSmartHomeApplianceDiscovery' smart constructor.
data StartSmartHomeApplianceDiscovery = StartSmartHomeApplianceDiscovery'
  { -- | The room where smart home appliance discovery was initiated.
    roomArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartSmartHomeApplianceDiscovery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'startSmartHomeApplianceDiscovery_roomArn' - The room where smart home appliance discovery was initiated.
newStartSmartHomeApplianceDiscovery ::
  -- | 'roomArn'
  Core.Text ->
  StartSmartHomeApplianceDiscovery
newStartSmartHomeApplianceDiscovery pRoomArn_ =
  StartSmartHomeApplianceDiscovery'
    { roomArn =
        pRoomArn_
    }

-- | The room where smart home appliance discovery was initiated.
startSmartHomeApplianceDiscovery_roomArn :: Lens.Lens' StartSmartHomeApplianceDiscovery Core.Text
startSmartHomeApplianceDiscovery_roomArn = Lens.lens (\StartSmartHomeApplianceDiscovery' {roomArn} -> roomArn) (\s@StartSmartHomeApplianceDiscovery' {} a -> s {roomArn = a} :: StartSmartHomeApplianceDiscovery)

instance
  Core.AWSRequest
    StartSmartHomeApplianceDiscovery
  where
  type
    AWSResponse StartSmartHomeApplianceDiscovery =
      StartSmartHomeApplianceDiscoveryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartSmartHomeApplianceDiscoveryResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    StartSmartHomeApplianceDiscovery

instance Core.NFData StartSmartHomeApplianceDiscovery

instance
  Core.ToHeaders
    StartSmartHomeApplianceDiscovery
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.StartSmartHomeApplianceDiscovery" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartSmartHomeApplianceDiscovery where
  toJSON StartSmartHomeApplianceDiscovery' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RoomArn" Core..= roomArn)]
      )

instance Core.ToPath StartSmartHomeApplianceDiscovery where
  toPath = Core.const "/"

instance
  Core.ToQuery
    StartSmartHomeApplianceDiscovery
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartSmartHomeApplianceDiscoveryResponse' smart constructor.
data StartSmartHomeApplianceDiscoveryResponse = StartSmartHomeApplianceDiscoveryResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartSmartHomeApplianceDiscoveryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startSmartHomeApplianceDiscoveryResponse_httpStatus' - The response's http status code.
newStartSmartHomeApplianceDiscoveryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartSmartHomeApplianceDiscoveryResponse
newStartSmartHomeApplianceDiscoveryResponse
  pHttpStatus_ =
    StartSmartHomeApplianceDiscoveryResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
startSmartHomeApplianceDiscoveryResponse_httpStatus :: Lens.Lens' StartSmartHomeApplianceDiscoveryResponse Core.Int
startSmartHomeApplianceDiscoveryResponse_httpStatus = Lens.lens (\StartSmartHomeApplianceDiscoveryResponse' {httpStatus} -> httpStatus) (\s@StartSmartHomeApplianceDiscoveryResponse' {} a -> s {httpStatus = a} :: StartSmartHomeApplianceDiscoveryResponse)

instance
  Core.NFData
    StartSmartHomeApplianceDiscoveryResponse
