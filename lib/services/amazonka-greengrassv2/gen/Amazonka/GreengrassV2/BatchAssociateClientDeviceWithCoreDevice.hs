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
-- Module      : Amazonka.GreengrassV2.BatchAssociateClientDeviceWithCoreDevice
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a list of client devices with a core device. Use this API
-- operation to specify which client devices can discover a core device
-- through cloud discovery. With cloud discovery, client devices connect to
-- IoT Greengrass to retrieve associated core devices\' connectivity
-- information and certificates. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/configure-cloud-discovery.html Configure cloud discovery>
-- in the /IoT Greengrass V2 Developer Guide/.
--
-- Client devices are local IoT devices that connect to and communicate
-- with an IoT Greengrass core device over MQTT. You can connect client
-- devices to a core device to sync MQTT messages and data to Amazon Web
-- Services IoT Core and interact with client devices in Greengrass
-- components. For more information, see
-- <https://docs.aws.amazon.com/greengrass/v2/developerguide/interact-with-local-iot-devices.html Interact with local IoT devices>
-- in the /IoT Greengrass V2 Developer Guide/.
module Amazonka.GreengrassV2.BatchAssociateClientDeviceWithCoreDevice
  ( -- * Creating a Request
    BatchAssociateClientDeviceWithCoreDevice (..),
    newBatchAssociateClientDeviceWithCoreDevice,

    -- * Request Lenses
    batchAssociateClientDeviceWithCoreDevice_entries,
    batchAssociateClientDeviceWithCoreDevice_coreDeviceThingName,

    -- * Destructuring the Response
    BatchAssociateClientDeviceWithCoreDeviceResponse (..),
    newBatchAssociateClientDeviceWithCoreDeviceResponse,

    -- * Response Lenses
    batchAssociateClientDeviceWithCoreDeviceResponse_errorEntries,
    batchAssociateClientDeviceWithCoreDeviceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchAssociateClientDeviceWithCoreDevice' smart constructor.
data BatchAssociateClientDeviceWithCoreDevice = BatchAssociateClientDeviceWithCoreDevice'
  { -- | The list of client devices to associate.
    entries :: Prelude.Maybe (Prelude.NonEmpty AssociateClientDeviceWithCoreDeviceEntry),
    -- | The name of the core device. This is also the name of the IoT thing.
    coreDeviceThingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateClientDeviceWithCoreDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entries', 'batchAssociateClientDeviceWithCoreDevice_entries' - The list of client devices to associate.
--
-- 'coreDeviceThingName', 'batchAssociateClientDeviceWithCoreDevice_coreDeviceThingName' - The name of the core device. This is also the name of the IoT thing.
newBatchAssociateClientDeviceWithCoreDevice ::
  -- | 'coreDeviceThingName'
  Prelude.Text ->
  BatchAssociateClientDeviceWithCoreDevice
newBatchAssociateClientDeviceWithCoreDevice
  pCoreDeviceThingName_ =
    BatchAssociateClientDeviceWithCoreDevice'
      { entries =
          Prelude.Nothing,
        coreDeviceThingName =
          pCoreDeviceThingName_
      }

-- | The list of client devices to associate.
batchAssociateClientDeviceWithCoreDevice_entries :: Lens.Lens' BatchAssociateClientDeviceWithCoreDevice (Prelude.Maybe (Prelude.NonEmpty AssociateClientDeviceWithCoreDeviceEntry))
batchAssociateClientDeviceWithCoreDevice_entries = Lens.lens (\BatchAssociateClientDeviceWithCoreDevice' {entries} -> entries) (\s@BatchAssociateClientDeviceWithCoreDevice' {} a -> s {entries = a} :: BatchAssociateClientDeviceWithCoreDevice) Prelude.. Lens.mapping Lens.coerced

-- | The name of the core device. This is also the name of the IoT thing.
batchAssociateClientDeviceWithCoreDevice_coreDeviceThingName :: Lens.Lens' BatchAssociateClientDeviceWithCoreDevice Prelude.Text
batchAssociateClientDeviceWithCoreDevice_coreDeviceThingName = Lens.lens (\BatchAssociateClientDeviceWithCoreDevice' {coreDeviceThingName} -> coreDeviceThingName) (\s@BatchAssociateClientDeviceWithCoreDevice' {} a -> s {coreDeviceThingName = a} :: BatchAssociateClientDeviceWithCoreDevice)

instance
  Core.AWSRequest
    BatchAssociateClientDeviceWithCoreDevice
  where
  type
    AWSResponse
      BatchAssociateClientDeviceWithCoreDevice =
      BatchAssociateClientDeviceWithCoreDeviceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchAssociateClientDeviceWithCoreDeviceResponse'
            Prelude.<$> (x Data..?> "errorEntries" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    BatchAssociateClientDeviceWithCoreDevice
  where
  hashWithSalt
    _salt
    BatchAssociateClientDeviceWithCoreDevice' {..} =
      _salt `Prelude.hashWithSalt` entries
        `Prelude.hashWithSalt` coreDeviceThingName

instance
  Prelude.NFData
    BatchAssociateClientDeviceWithCoreDevice
  where
  rnf BatchAssociateClientDeviceWithCoreDevice' {..} =
    Prelude.rnf entries
      `Prelude.seq` Prelude.rnf coreDeviceThingName

instance
  Data.ToHeaders
    BatchAssociateClientDeviceWithCoreDevice
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    BatchAssociateClientDeviceWithCoreDevice
  where
  toJSON BatchAssociateClientDeviceWithCoreDevice' {..} =
    Data.object
      ( Prelude.catMaybes
          [("entries" Data..=) Prelude.<$> entries]
      )

instance
  Data.ToPath
    BatchAssociateClientDeviceWithCoreDevice
  where
  toPath BatchAssociateClientDeviceWithCoreDevice' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/coreDevices/",
        Data.toBS coreDeviceThingName,
        "/associateClientDevices"
      ]

instance
  Data.ToQuery
    BatchAssociateClientDeviceWithCoreDevice
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchAssociateClientDeviceWithCoreDeviceResponse' smart constructor.
data BatchAssociateClientDeviceWithCoreDeviceResponse = BatchAssociateClientDeviceWithCoreDeviceResponse'
  { -- | The list of any errors for the entries in the request. Each error entry
    -- contains the name of the IoT thing that failed to associate.
    errorEntries :: Prelude.Maybe [AssociateClientDeviceWithCoreDeviceErrorEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchAssociateClientDeviceWithCoreDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorEntries', 'batchAssociateClientDeviceWithCoreDeviceResponse_errorEntries' - The list of any errors for the entries in the request. Each error entry
-- contains the name of the IoT thing that failed to associate.
--
-- 'httpStatus', 'batchAssociateClientDeviceWithCoreDeviceResponse_httpStatus' - The response's http status code.
newBatchAssociateClientDeviceWithCoreDeviceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchAssociateClientDeviceWithCoreDeviceResponse
newBatchAssociateClientDeviceWithCoreDeviceResponse
  pHttpStatus_ =
    BatchAssociateClientDeviceWithCoreDeviceResponse'
      { errorEntries =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The list of any errors for the entries in the request. Each error entry
-- contains the name of the IoT thing that failed to associate.
batchAssociateClientDeviceWithCoreDeviceResponse_errorEntries :: Lens.Lens' BatchAssociateClientDeviceWithCoreDeviceResponse (Prelude.Maybe [AssociateClientDeviceWithCoreDeviceErrorEntry])
batchAssociateClientDeviceWithCoreDeviceResponse_errorEntries = Lens.lens (\BatchAssociateClientDeviceWithCoreDeviceResponse' {errorEntries} -> errorEntries) (\s@BatchAssociateClientDeviceWithCoreDeviceResponse' {} a -> s {errorEntries = a} :: BatchAssociateClientDeviceWithCoreDeviceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchAssociateClientDeviceWithCoreDeviceResponse_httpStatus :: Lens.Lens' BatchAssociateClientDeviceWithCoreDeviceResponse Prelude.Int
batchAssociateClientDeviceWithCoreDeviceResponse_httpStatus = Lens.lens (\BatchAssociateClientDeviceWithCoreDeviceResponse' {httpStatus} -> httpStatus) (\s@BatchAssociateClientDeviceWithCoreDeviceResponse' {} a -> s {httpStatus = a} :: BatchAssociateClientDeviceWithCoreDeviceResponse)

instance
  Prelude.NFData
    BatchAssociateClientDeviceWithCoreDeviceResponse
  where
  rnf
    BatchAssociateClientDeviceWithCoreDeviceResponse' {..} =
      Prelude.rnf errorEntries
        `Prelude.seq` Prelude.rnf httpStatus
