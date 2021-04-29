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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartSmartHomeApplianceDiscovery' smart constructor.
data StartSmartHomeApplianceDiscovery = StartSmartHomeApplianceDiscovery'
  { -- | The room where smart home appliance discovery was initiated.
    roomArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  StartSmartHomeApplianceDiscovery
newStartSmartHomeApplianceDiscovery pRoomArn_ =
  StartSmartHomeApplianceDiscovery'
    { roomArn =
        pRoomArn_
    }

-- | The room where smart home appliance discovery was initiated.
startSmartHomeApplianceDiscovery_roomArn :: Lens.Lens' StartSmartHomeApplianceDiscovery Prelude.Text
startSmartHomeApplianceDiscovery_roomArn = Lens.lens (\StartSmartHomeApplianceDiscovery' {roomArn} -> roomArn) (\s@StartSmartHomeApplianceDiscovery' {} a -> s {roomArn = a} :: StartSmartHomeApplianceDiscovery)

instance
  Prelude.AWSRequest
    StartSmartHomeApplianceDiscovery
  where
  type
    Rs StartSmartHomeApplianceDiscovery =
      StartSmartHomeApplianceDiscoveryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartSmartHomeApplianceDiscoveryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartSmartHomeApplianceDiscovery

instance
  Prelude.NFData
    StartSmartHomeApplianceDiscovery

instance
  Prelude.ToHeaders
    StartSmartHomeApplianceDiscovery
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.StartSmartHomeApplianceDiscovery" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    StartSmartHomeApplianceDiscovery
  where
  toJSON StartSmartHomeApplianceDiscovery' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("RoomArn" Prelude..= roomArn)]
      )

instance
  Prelude.ToPath
    StartSmartHomeApplianceDiscovery
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    StartSmartHomeApplianceDiscovery
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSmartHomeApplianceDiscoveryResponse' smart constructor.
data StartSmartHomeApplianceDiscoveryResponse = StartSmartHomeApplianceDiscoveryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  StartSmartHomeApplianceDiscoveryResponse
newStartSmartHomeApplianceDiscoveryResponse
  pHttpStatus_ =
    StartSmartHomeApplianceDiscoveryResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
startSmartHomeApplianceDiscoveryResponse_httpStatus :: Lens.Lens' StartSmartHomeApplianceDiscoveryResponse Prelude.Int
startSmartHomeApplianceDiscoveryResponse_httpStatus = Lens.lens (\StartSmartHomeApplianceDiscoveryResponse' {httpStatus} -> httpStatus) (\s@StartSmartHomeApplianceDiscoveryResponse' {} a -> s {httpStatus = a} :: StartSmartHomeApplianceDiscoveryResponse)

instance
  Prelude.NFData
    StartSmartHomeApplianceDiscoveryResponse
