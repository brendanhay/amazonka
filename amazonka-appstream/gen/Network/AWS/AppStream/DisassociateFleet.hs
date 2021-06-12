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
-- Module      : Network.AWS.AppStream.DisassociateFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the specified fleet from the specified stack.
module Network.AWS.AppStream.DisassociateFleet
  ( -- * Creating a Request
    DisassociateFleet (..),
    newDisassociateFleet,

    -- * Request Lenses
    disassociateFleet_fleetName,
    disassociateFleet_stackName,

    -- * Destructuring the Response
    DisassociateFleetResponse (..),
    newDisassociateFleetResponse,

    -- * Response Lenses
    disassociateFleetResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisassociateFleet' smart constructor.
data DisassociateFleet = DisassociateFleet'
  { -- | The name of the fleet.
    fleetName :: Core.Text,
    -- | The name of the stack.
    stackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetName', 'disassociateFleet_fleetName' - The name of the fleet.
--
-- 'stackName', 'disassociateFleet_stackName' - The name of the stack.
newDisassociateFleet ::
  -- | 'fleetName'
  Core.Text ->
  -- | 'stackName'
  Core.Text ->
  DisassociateFleet
newDisassociateFleet pFleetName_ pStackName_ =
  DisassociateFleet'
    { fleetName = pFleetName_,
      stackName = pStackName_
    }

-- | The name of the fleet.
disassociateFleet_fleetName :: Lens.Lens' DisassociateFleet Core.Text
disassociateFleet_fleetName = Lens.lens (\DisassociateFleet' {fleetName} -> fleetName) (\s@DisassociateFleet' {} a -> s {fleetName = a} :: DisassociateFleet)

-- | The name of the stack.
disassociateFleet_stackName :: Lens.Lens' DisassociateFleet Core.Text
disassociateFleet_stackName = Lens.lens (\DisassociateFleet' {stackName} -> stackName) (\s@DisassociateFleet' {} a -> s {stackName = a} :: DisassociateFleet)

instance Core.AWSRequest DisassociateFleet where
  type
    AWSResponse DisassociateFleet =
      DisassociateFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateFleetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DisassociateFleet

instance Core.NFData DisassociateFleet

instance Core.ToHeaders DisassociateFleet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.DisassociateFleet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DisassociateFleet where
  toJSON DisassociateFleet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetName" Core..= fleetName),
            Core.Just ("StackName" Core..= stackName)
          ]
      )

instance Core.ToPath DisassociateFleet where
  toPath = Core.const "/"

instance Core.ToQuery DisassociateFleet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDisassociateFleetResponse' smart constructor.
data DisassociateFleetResponse = DisassociateFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateFleetResponse_httpStatus' - The response's http status code.
newDisassociateFleetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisassociateFleetResponse
newDisassociateFleetResponse pHttpStatus_ =
  DisassociateFleetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
disassociateFleetResponse_httpStatus :: Lens.Lens' DisassociateFleetResponse Core.Int
disassociateFleetResponse_httpStatus = Lens.lens (\DisassociateFleetResponse' {httpStatus} -> httpStatus) (\s@DisassociateFleetResponse' {} a -> s {httpStatus = a} :: DisassociateFleetResponse)

instance Core.NFData DisassociateFleetResponse
