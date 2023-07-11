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
-- Module      : Amazonka.GreengrassV2.DeleteCoreDevice
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Greengrass core device, which is an IoT thing. This operation
-- removes the core device from the list of core devices. This operation
-- doesn\'t delete the IoT thing. For more information about how to delete
-- the IoT thing, see
-- <https://docs.aws.amazon.com/iot/latest/apireference/API_DeleteThing.html DeleteThing>
-- in the /IoT API Reference/.
module Amazonka.GreengrassV2.DeleteCoreDevice
  ( -- * Creating a Request
    DeleteCoreDevice (..),
    newDeleteCoreDevice,

    -- * Request Lenses
    deleteCoreDevice_coreDeviceThingName,

    -- * Destructuring the Response
    DeleteCoreDeviceResponse (..),
    newDeleteCoreDeviceResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteCoreDevice' smart constructor.
data DeleteCoreDevice = DeleteCoreDevice'
  { -- | The name of the core device. This is also the name of the IoT thing.
    coreDeviceThingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoreDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'coreDeviceThingName', 'deleteCoreDevice_coreDeviceThingName' - The name of the core device. This is also the name of the IoT thing.
newDeleteCoreDevice ::
  -- | 'coreDeviceThingName'
  Prelude.Text ->
  DeleteCoreDevice
newDeleteCoreDevice pCoreDeviceThingName_ =
  DeleteCoreDevice'
    { coreDeviceThingName =
        pCoreDeviceThingName_
    }

-- | The name of the core device. This is also the name of the IoT thing.
deleteCoreDevice_coreDeviceThingName :: Lens.Lens' DeleteCoreDevice Prelude.Text
deleteCoreDevice_coreDeviceThingName = Lens.lens (\DeleteCoreDevice' {coreDeviceThingName} -> coreDeviceThingName) (\s@DeleteCoreDevice' {} a -> s {coreDeviceThingName = a} :: DeleteCoreDevice)

instance Core.AWSRequest DeleteCoreDevice where
  type
    AWSResponse DeleteCoreDevice =
      DeleteCoreDeviceResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteCoreDeviceResponse'

instance Prelude.Hashable DeleteCoreDevice where
  hashWithSalt _salt DeleteCoreDevice' {..} =
    _salt `Prelude.hashWithSalt` coreDeviceThingName

instance Prelude.NFData DeleteCoreDevice where
  rnf DeleteCoreDevice' {..} =
    Prelude.rnf coreDeviceThingName

instance Data.ToHeaders DeleteCoreDevice where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteCoreDevice where
  toPath DeleteCoreDevice' {..} =
    Prelude.mconcat
      [ "/greengrass/v2/coreDevices/",
        Data.toBS coreDeviceThingName
      ]

instance Data.ToQuery DeleteCoreDevice where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteCoreDeviceResponse' smart constructor.
data DeleteCoreDeviceResponse = DeleteCoreDeviceResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCoreDeviceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteCoreDeviceResponse ::
  DeleteCoreDeviceResponse
newDeleteCoreDeviceResponse =
  DeleteCoreDeviceResponse'

instance Prelude.NFData DeleteCoreDeviceResponse where
  rnf _ = ()
