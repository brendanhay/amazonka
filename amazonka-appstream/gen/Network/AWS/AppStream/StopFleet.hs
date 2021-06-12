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
-- Module      : Network.AWS.AppStream.StopFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops the specified fleet.
module Network.AWS.AppStream.StopFleet
  ( -- * Creating a Request
    StopFleet (..),
    newStopFleet,

    -- * Request Lenses
    stopFleet_name,

    -- * Destructuring the Response
    StopFleetResponse (..),
    newStopFleetResponse,

    -- * Response Lenses
    stopFleetResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStopFleet' smart constructor.
data StopFleet = StopFleet'
  { -- | The name of the fleet.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'stopFleet_name' - The name of the fleet.
newStopFleet ::
  -- | 'name'
  Core.Text ->
  StopFleet
newStopFleet pName_ = StopFleet' {name = pName_}

-- | The name of the fleet.
stopFleet_name :: Lens.Lens' StopFleet Core.Text
stopFleet_name = Lens.lens (\StopFleet' {name} -> name) (\s@StopFleet' {} a -> s {name = a} :: StopFleet)

instance Core.AWSRequest StopFleet where
  type AWSResponse StopFleet = StopFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopFleetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StopFleet

instance Core.NFData StopFleet

instance Core.ToHeaders StopFleet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.StopFleet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StopFleet where
  toJSON StopFleet' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath StopFleet where
  toPath = Core.const "/"

instance Core.ToQuery StopFleet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStopFleetResponse' smart constructor.
data StopFleetResponse = StopFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StopFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'stopFleetResponse_httpStatus' - The response's http status code.
newStopFleetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StopFleetResponse
newStopFleetResponse pHttpStatus_ =
  StopFleetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
stopFleetResponse_httpStatus :: Lens.Lens' StopFleetResponse Core.Int
stopFleetResponse_httpStatus = Lens.lens (\StopFleetResponse' {httpStatus} -> httpStatus) (\s@StopFleetResponse' {} a -> s {httpStatus = a} :: StopFleetResponse)

instance Core.NFData StopFleetResponse
