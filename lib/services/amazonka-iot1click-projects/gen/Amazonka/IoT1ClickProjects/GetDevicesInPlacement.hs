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
-- Module      : Amazonka.IoT1ClickProjects.GetDevicesInPlacement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an object enumerating the devices in a placement.
module Amazonka.IoT1ClickProjects.GetDevicesInPlacement
  ( -- * Creating a Request
    GetDevicesInPlacement (..),
    newGetDevicesInPlacement,

    -- * Request Lenses
    getDevicesInPlacement_projectName,
    getDevicesInPlacement_placementName,

    -- * Destructuring the Response
    GetDevicesInPlacementResponse (..),
    newGetDevicesInPlacementResponse,

    -- * Response Lenses
    getDevicesInPlacementResponse_httpStatus,
    getDevicesInPlacementResponse_devices,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickProjects.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDevicesInPlacement' smart constructor.
data GetDevicesInPlacement = GetDevicesInPlacement'
  { -- | The name of the project containing the placement.
    projectName :: Prelude.Text,
    -- | The name of the placement to get the devices from.
    placementName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevicesInPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'getDevicesInPlacement_projectName' - The name of the project containing the placement.
--
-- 'placementName', 'getDevicesInPlacement_placementName' - The name of the placement to get the devices from.
newGetDevicesInPlacement ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'placementName'
  Prelude.Text ->
  GetDevicesInPlacement
newGetDevicesInPlacement
  pProjectName_
  pPlacementName_ =
    GetDevicesInPlacement'
      { projectName = pProjectName_,
        placementName = pPlacementName_
      }

-- | The name of the project containing the placement.
getDevicesInPlacement_projectName :: Lens.Lens' GetDevicesInPlacement Prelude.Text
getDevicesInPlacement_projectName = Lens.lens (\GetDevicesInPlacement' {projectName} -> projectName) (\s@GetDevicesInPlacement' {} a -> s {projectName = a} :: GetDevicesInPlacement)

-- | The name of the placement to get the devices from.
getDevicesInPlacement_placementName :: Lens.Lens' GetDevicesInPlacement Prelude.Text
getDevicesInPlacement_placementName = Lens.lens (\GetDevicesInPlacement' {placementName} -> placementName) (\s@GetDevicesInPlacement' {} a -> s {placementName = a} :: GetDevicesInPlacement)

instance Core.AWSRequest GetDevicesInPlacement where
  type
    AWSResponse GetDevicesInPlacement =
      GetDevicesInPlacementResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDevicesInPlacementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "devices" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetDevicesInPlacement where
  hashWithSalt _salt GetDevicesInPlacement' {..} =
    _salt `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` placementName

instance Prelude.NFData GetDevicesInPlacement where
  rnf GetDevicesInPlacement' {..} =
    Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf placementName

instance Data.ToHeaders GetDevicesInPlacement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDevicesInPlacement where
  toPath GetDevicesInPlacement' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS projectName,
        "/placements/",
        Data.toBS placementName,
        "/devices"
      ]

instance Data.ToQuery GetDevicesInPlacement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDevicesInPlacementResponse' smart constructor.
data GetDevicesInPlacementResponse = GetDevicesInPlacementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An object containing the devices (zero or more) within the placement.
    devices :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDevicesInPlacementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDevicesInPlacementResponse_httpStatus' - The response's http status code.
--
-- 'devices', 'getDevicesInPlacementResponse_devices' - An object containing the devices (zero or more) within the placement.
newGetDevicesInPlacementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDevicesInPlacementResponse
newGetDevicesInPlacementResponse pHttpStatus_ =
  GetDevicesInPlacementResponse'
    { httpStatus =
        pHttpStatus_,
      devices = Prelude.mempty
    }

-- | The response's http status code.
getDevicesInPlacementResponse_httpStatus :: Lens.Lens' GetDevicesInPlacementResponse Prelude.Int
getDevicesInPlacementResponse_httpStatus = Lens.lens (\GetDevicesInPlacementResponse' {httpStatus} -> httpStatus) (\s@GetDevicesInPlacementResponse' {} a -> s {httpStatus = a} :: GetDevicesInPlacementResponse)

-- | An object containing the devices (zero or more) within the placement.
getDevicesInPlacementResponse_devices :: Lens.Lens' GetDevicesInPlacementResponse (Prelude.HashMap Prelude.Text Prelude.Text)
getDevicesInPlacementResponse_devices = Lens.lens (\GetDevicesInPlacementResponse' {devices} -> devices) (\s@GetDevicesInPlacementResponse' {} a -> s {devices = a} :: GetDevicesInPlacementResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetDevicesInPlacementResponse where
  rnf GetDevicesInPlacementResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf devices
