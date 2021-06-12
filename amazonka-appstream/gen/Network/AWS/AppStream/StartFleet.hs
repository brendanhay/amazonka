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
-- Module      : Network.AWS.AppStream.StartFleet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the specified fleet.
module Network.AWS.AppStream.StartFleet
  ( -- * Creating a Request
    StartFleet (..),
    newStartFleet,

    -- * Request Lenses
    startFleet_name,

    -- * Destructuring the Response
    StartFleetResponse (..),
    newStartFleetResponse,

    -- * Response Lenses
    startFleetResponse_httpStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newStartFleet' smart constructor.
data StartFleet = StartFleet'
  { -- | The name of the fleet.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'startFleet_name' - The name of the fleet.
newStartFleet ::
  -- | 'name'
  Core.Text ->
  StartFleet
newStartFleet pName_ = StartFleet' {name = pName_}

-- | The name of the fleet.
startFleet_name :: Lens.Lens' StartFleet Core.Text
startFleet_name = Lens.lens (\StartFleet' {name} -> name) (\s@StartFleet' {} a -> s {name = a} :: StartFleet)

instance Core.AWSRequest StartFleet where
  type AWSResponse StartFleet = StartFleetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartFleetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable StartFleet

instance Core.NFData StartFleet

instance Core.ToHeaders StartFleet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "PhotonAdminProxyService.StartFleet" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON StartFleet where
  toJSON StartFleet' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath StartFleet where
  toPath = Core.const "/"

instance Core.ToQuery StartFleet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newStartFleetResponse' smart constructor.
data StartFleetResponse = StartFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'StartFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startFleetResponse_httpStatus' - The response's http status code.
newStartFleetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  StartFleetResponse
newStartFleetResponse pHttpStatus_ =
  StartFleetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
startFleetResponse_httpStatus :: Lens.Lens' StartFleetResponse Core.Int
startFleetResponse_httpStatus = Lens.lens (\StartFleetResponse' {httpStatus} -> httpStatus) (\s@StartFleetResponse' {} a -> s {httpStatus = a} :: StartFleetResponse)

instance Core.NFData StartFleetResponse
