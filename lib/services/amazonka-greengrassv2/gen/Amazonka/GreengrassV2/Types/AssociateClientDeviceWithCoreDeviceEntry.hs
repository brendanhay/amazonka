{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GreengrassV2.Types.AssociateClientDeviceWithCoreDeviceEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.AssociateClientDeviceWithCoreDeviceEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a request to associate a client device with a core device. The
-- <https://docs.aws.amazon.com/greengrass/v2/APIReference/API_BatchAssociateClientDeviceWithCoreDevice.html BatchAssociateClientDeviceWithCoreDevice>
-- operation consumes a list of these requests.
--
-- /See:/ 'newAssociateClientDeviceWithCoreDeviceEntry' smart constructor.
data AssociateClientDeviceWithCoreDeviceEntry = AssociateClientDeviceWithCoreDeviceEntry'
  { -- | The name of the IoT thing that represents the client device to
    -- associate.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateClientDeviceWithCoreDeviceEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'associateClientDeviceWithCoreDeviceEntry_thingName' - The name of the IoT thing that represents the client device to
-- associate.
newAssociateClientDeviceWithCoreDeviceEntry ::
  -- | 'thingName'
  Prelude.Text ->
  AssociateClientDeviceWithCoreDeviceEntry
newAssociateClientDeviceWithCoreDeviceEntry
  pThingName_ =
    AssociateClientDeviceWithCoreDeviceEntry'
      { thingName =
          pThingName_
      }

-- | The name of the IoT thing that represents the client device to
-- associate.
associateClientDeviceWithCoreDeviceEntry_thingName :: Lens.Lens' AssociateClientDeviceWithCoreDeviceEntry Prelude.Text
associateClientDeviceWithCoreDeviceEntry_thingName = Lens.lens (\AssociateClientDeviceWithCoreDeviceEntry' {thingName} -> thingName) (\s@AssociateClientDeviceWithCoreDeviceEntry' {} a -> s {thingName = a} :: AssociateClientDeviceWithCoreDeviceEntry)

instance
  Prelude.Hashable
    AssociateClientDeviceWithCoreDeviceEntry
  where
  hashWithSalt
    _salt
    AssociateClientDeviceWithCoreDeviceEntry' {..} =
      _salt `Prelude.hashWithSalt` thingName

instance
  Prelude.NFData
    AssociateClientDeviceWithCoreDeviceEntry
  where
  rnf AssociateClientDeviceWithCoreDeviceEntry' {..} =
    Prelude.rnf thingName

instance
  Data.ToJSON
    AssociateClientDeviceWithCoreDeviceEntry
  where
  toJSON AssociateClientDeviceWithCoreDeviceEntry' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("thingName" Data..= thingName)]
      )
