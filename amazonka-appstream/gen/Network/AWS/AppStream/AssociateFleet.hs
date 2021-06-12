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
-- Module      : Network.AWS.AppStream.AssociateFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified fleet with the specified stack.
module Network.AWS.AppStream.AssociateFleet
  ( -- * Creating a Request
    AssociateFleet (..),
    newAssociateFleet,

    -- * Request Lenses
    associateFleet_fleetName,
    associateFleet_stackName,

    -- * Destructuring the Response
    AssociateFleetResponse (..),
    newAssociateFleetResponse,

    -- * Response Lenses
    associateFleetResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAssociateFleet' smart constructor.
data AssociateFleet = AssociateFleet'
  { -- | The name of the fleet.
    fleetName :: Core.Text,
    -- | The name of the stack.
    stackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetName', 'associateFleet_fleetName' - The name of the fleet.
--
-- 'stackName', 'associateFleet_stackName' - The name of the stack.
newAssociateFleet ::
  -- | 'fleetName'
  Core.Text ->
  -- | 'stackName'
  Core.Text ->
  AssociateFleet
newAssociateFleet pFleetName_ pStackName_ =
  AssociateFleet'
    { fleetName = pFleetName_,
      stackName = pStackName_
    }

-- | The name of the fleet.
associateFleet_fleetName :: Lens.Lens' AssociateFleet Core.Text
associateFleet_fleetName = Lens.lens (\AssociateFleet' {fleetName} -> fleetName) (\s@AssociateFleet' {} a -> s {fleetName = a} :: AssociateFleet)

-- | The name of the stack.
associateFleet_stackName :: Lens.Lens' AssociateFleet Core.Text
associateFleet_stackName = Lens.lens (\AssociateFleet' {stackName} -> stackName) (\s@AssociateFleet' {} a -> s {stackName = a} :: AssociateFleet)

instance Core.AWSRequest AssociateFleet where
  type
    AWSResponse AssociateFleet =
      AssociateFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateFleetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AssociateFleet

instance Core.NFData AssociateFleet

instance Core.ToHeaders AssociateFleet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.AssociateFleet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON AssociateFleet where
  toJSON AssociateFleet' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("FleetName" Core..= fleetName),
            Core.Just ("StackName" Core..= stackName)
          ]
      )

instance Core.ToPath AssociateFleet where
  toPath = Core.const "/"

instance Core.ToQuery AssociateFleet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAssociateFleetResponse' smart constructor.
data AssociateFleetResponse = AssociateFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AssociateFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateFleetResponse_httpStatus' - The response's http status code.
newAssociateFleetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AssociateFleetResponse
newAssociateFleetResponse pHttpStatus_ =
  AssociateFleetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
associateFleetResponse_httpStatus :: Lens.Lens' AssociateFleetResponse Core.Int
associateFleetResponse_httpStatus = Lens.lens (\AssociateFleetResponse' {httpStatus} -> httpStatus) (\s@AssociateFleetResponse' {} a -> s {httpStatus = a} :: AssociateFleetResponse)

instance Core.NFData AssociateFleetResponse
