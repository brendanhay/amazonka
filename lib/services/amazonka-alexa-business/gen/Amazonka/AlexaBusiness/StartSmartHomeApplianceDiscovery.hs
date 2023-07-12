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
-- Module      : Amazonka.AlexaBusiness.StartSmartHomeApplianceDiscovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the discovery of any smart home appliances associated with the
-- room.
module Amazonka.AlexaBusiness.StartSmartHomeApplianceDiscovery
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

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newStartSmartHomeApplianceDiscovery' smart constructor.
data StartSmartHomeApplianceDiscovery = StartSmartHomeApplianceDiscovery'
  { -- | The room where smart home appliance discovery was initiated.
    roomArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    StartSmartHomeApplianceDiscovery
  where
  type
    AWSResponse StartSmartHomeApplianceDiscovery =
      StartSmartHomeApplianceDiscoveryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartSmartHomeApplianceDiscoveryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    StartSmartHomeApplianceDiscovery
  where
  hashWithSalt
    _salt
    StartSmartHomeApplianceDiscovery' {..} =
      _salt `Prelude.hashWithSalt` roomArn

instance
  Prelude.NFData
    StartSmartHomeApplianceDiscovery
  where
  rnf StartSmartHomeApplianceDiscovery' {..} =
    Prelude.rnf roomArn

instance
  Data.ToHeaders
    StartSmartHomeApplianceDiscovery
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.StartSmartHomeApplianceDiscovery" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartSmartHomeApplianceDiscovery where
  toJSON StartSmartHomeApplianceDiscovery' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RoomArn" Data..= roomArn)]
      )

instance Data.ToPath StartSmartHomeApplianceDiscovery where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    StartSmartHomeApplianceDiscovery
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartSmartHomeApplianceDiscoveryResponse' smart constructor.
data StartSmartHomeApplianceDiscoveryResponse = StartSmartHomeApplianceDiscoveryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf StartSmartHomeApplianceDiscoveryResponse' {..} =
    Prelude.rnf httpStatus
