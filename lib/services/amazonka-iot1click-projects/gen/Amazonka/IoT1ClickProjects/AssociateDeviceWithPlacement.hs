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
-- Module      : Amazonka.IoT1ClickProjects.AssociateDeviceWithPlacement
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a physical device with a placement.
module Amazonka.IoT1ClickProjects.AssociateDeviceWithPlacement
  ( -- * Creating a Request
    AssociateDeviceWithPlacement (..),
    newAssociateDeviceWithPlacement,

    -- * Request Lenses
    associateDeviceWithPlacement_projectName,
    associateDeviceWithPlacement_placementName,
    associateDeviceWithPlacement_deviceId,
    associateDeviceWithPlacement_deviceTemplateName,

    -- * Destructuring the Response
    AssociateDeviceWithPlacementResponse (..),
    newAssociateDeviceWithPlacementResponse,

    -- * Response Lenses
    associateDeviceWithPlacementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickProjects.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateDeviceWithPlacement' smart constructor.
data AssociateDeviceWithPlacement = AssociateDeviceWithPlacement'
  { -- | The name of the project containing the placement in which to associate
    -- the device.
    projectName :: Prelude.Text,
    -- | The name of the placement in which to associate the device.
    placementName :: Prelude.Text,
    -- | The ID of the physical device to be associated with the given placement
    -- in the project. Note that a mandatory 4 character prefix is required for
    -- all @deviceId@ values.
    deviceId :: Prelude.Text,
    -- | The device template name to associate with the device ID.
    deviceTemplateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDeviceWithPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'projectName', 'associateDeviceWithPlacement_projectName' - The name of the project containing the placement in which to associate
-- the device.
--
-- 'placementName', 'associateDeviceWithPlacement_placementName' - The name of the placement in which to associate the device.
--
-- 'deviceId', 'associateDeviceWithPlacement_deviceId' - The ID of the physical device to be associated with the given placement
-- in the project. Note that a mandatory 4 character prefix is required for
-- all @deviceId@ values.
--
-- 'deviceTemplateName', 'associateDeviceWithPlacement_deviceTemplateName' - The device template name to associate with the device ID.
newAssociateDeviceWithPlacement ::
  -- | 'projectName'
  Prelude.Text ->
  -- | 'placementName'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  -- | 'deviceTemplateName'
  Prelude.Text ->
  AssociateDeviceWithPlacement
newAssociateDeviceWithPlacement
  pProjectName_
  pPlacementName_
  pDeviceId_
  pDeviceTemplateName_ =
    AssociateDeviceWithPlacement'
      { projectName =
          pProjectName_,
        placementName = pPlacementName_,
        deviceId = pDeviceId_,
        deviceTemplateName = pDeviceTemplateName_
      }

-- | The name of the project containing the placement in which to associate
-- the device.
associateDeviceWithPlacement_projectName :: Lens.Lens' AssociateDeviceWithPlacement Prelude.Text
associateDeviceWithPlacement_projectName = Lens.lens (\AssociateDeviceWithPlacement' {projectName} -> projectName) (\s@AssociateDeviceWithPlacement' {} a -> s {projectName = a} :: AssociateDeviceWithPlacement)

-- | The name of the placement in which to associate the device.
associateDeviceWithPlacement_placementName :: Lens.Lens' AssociateDeviceWithPlacement Prelude.Text
associateDeviceWithPlacement_placementName = Lens.lens (\AssociateDeviceWithPlacement' {placementName} -> placementName) (\s@AssociateDeviceWithPlacement' {} a -> s {placementName = a} :: AssociateDeviceWithPlacement)

-- | The ID of the physical device to be associated with the given placement
-- in the project. Note that a mandatory 4 character prefix is required for
-- all @deviceId@ values.
associateDeviceWithPlacement_deviceId :: Lens.Lens' AssociateDeviceWithPlacement Prelude.Text
associateDeviceWithPlacement_deviceId = Lens.lens (\AssociateDeviceWithPlacement' {deviceId} -> deviceId) (\s@AssociateDeviceWithPlacement' {} a -> s {deviceId = a} :: AssociateDeviceWithPlacement)

-- | The device template name to associate with the device ID.
associateDeviceWithPlacement_deviceTemplateName :: Lens.Lens' AssociateDeviceWithPlacement Prelude.Text
associateDeviceWithPlacement_deviceTemplateName = Lens.lens (\AssociateDeviceWithPlacement' {deviceTemplateName} -> deviceTemplateName) (\s@AssociateDeviceWithPlacement' {} a -> s {deviceTemplateName = a} :: AssociateDeviceWithPlacement)

instance Core.AWSRequest AssociateDeviceWithPlacement where
  type
    AWSResponse AssociateDeviceWithPlacement =
      AssociateDeviceWithPlacementResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateDeviceWithPlacementResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateDeviceWithPlacement
  where
  hashWithSalt _salt AssociateDeviceWithPlacement' {..} =
    _salt `Prelude.hashWithSalt` projectName
      `Prelude.hashWithSalt` placementName
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` deviceTemplateName

instance Prelude.NFData AssociateDeviceWithPlacement where
  rnf AssociateDeviceWithPlacement' {..} =
    Prelude.rnf projectName
      `Prelude.seq` Prelude.rnf placementName
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf deviceTemplateName

instance Data.ToHeaders AssociateDeviceWithPlacement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateDeviceWithPlacement where
  toJSON AssociateDeviceWithPlacement' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("deviceId" Data..= deviceId)]
      )

instance Data.ToPath AssociateDeviceWithPlacement where
  toPath AssociateDeviceWithPlacement' {..} =
    Prelude.mconcat
      [ "/projects/",
        Data.toBS projectName,
        "/placements/",
        Data.toBS placementName,
        "/devices/",
        Data.toBS deviceTemplateName
      ]

instance Data.ToQuery AssociateDeviceWithPlacement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateDeviceWithPlacementResponse' smart constructor.
data AssociateDeviceWithPlacementResponse = AssociateDeviceWithPlacementResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateDeviceWithPlacementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateDeviceWithPlacementResponse_httpStatus' - The response's http status code.
newAssociateDeviceWithPlacementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateDeviceWithPlacementResponse
newAssociateDeviceWithPlacementResponse pHttpStatus_ =
  AssociateDeviceWithPlacementResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
associateDeviceWithPlacementResponse_httpStatus :: Lens.Lens' AssociateDeviceWithPlacementResponse Prelude.Int
associateDeviceWithPlacementResponse_httpStatus = Lens.lens (\AssociateDeviceWithPlacementResponse' {httpStatus} -> httpStatus) (\s@AssociateDeviceWithPlacementResponse' {} a -> s {httpStatus = a} :: AssociateDeviceWithPlacementResponse)

instance
  Prelude.NFData
    AssociateDeviceWithPlacementResponse
  where
  rnf AssociateDeviceWithPlacementResponse' {..} =
    Prelude.rnf httpStatus
