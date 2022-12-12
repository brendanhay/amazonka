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
-- Module      : Amazonka.GreengrassV2.Types.AssociateClientDeviceWithCoreDeviceErrorEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.AssociateClientDeviceWithCoreDeviceErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an error that occurs from a request to associate a client
-- device with a core device. The
-- <https://docs.aws.amazon.com/greengrass/v2/APIReference/API_BatchAssociateClientDeviceWithCoreDevice.html BatchAssociateClientDeviceWithCoreDevice>
-- operation returns a list of these errors.
--
-- /See:/ 'newAssociateClientDeviceWithCoreDeviceErrorEntry' smart constructor.
data AssociateClientDeviceWithCoreDeviceErrorEntry = AssociateClientDeviceWithCoreDeviceErrorEntry'
  { -- | The error code for the request.
    code :: Prelude.Maybe Prelude.Text,
    -- | A message that provides additional information about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the IoT thing whose associate request failed.
    thingName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateClientDeviceWithCoreDeviceErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'associateClientDeviceWithCoreDeviceErrorEntry_code' - The error code for the request.
--
-- 'message', 'associateClientDeviceWithCoreDeviceErrorEntry_message' - A message that provides additional information about the error.
--
-- 'thingName', 'associateClientDeviceWithCoreDeviceErrorEntry_thingName' - The name of the IoT thing whose associate request failed.
newAssociateClientDeviceWithCoreDeviceErrorEntry ::
  AssociateClientDeviceWithCoreDeviceErrorEntry
newAssociateClientDeviceWithCoreDeviceErrorEntry =
  AssociateClientDeviceWithCoreDeviceErrorEntry'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing,
      thingName = Prelude.Nothing
    }

-- | The error code for the request.
associateClientDeviceWithCoreDeviceErrorEntry_code :: Lens.Lens' AssociateClientDeviceWithCoreDeviceErrorEntry (Prelude.Maybe Prelude.Text)
associateClientDeviceWithCoreDeviceErrorEntry_code = Lens.lens (\AssociateClientDeviceWithCoreDeviceErrorEntry' {code} -> code) (\s@AssociateClientDeviceWithCoreDeviceErrorEntry' {} a -> s {code = a} :: AssociateClientDeviceWithCoreDeviceErrorEntry)

-- | A message that provides additional information about the error.
associateClientDeviceWithCoreDeviceErrorEntry_message :: Lens.Lens' AssociateClientDeviceWithCoreDeviceErrorEntry (Prelude.Maybe Prelude.Text)
associateClientDeviceWithCoreDeviceErrorEntry_message = Lens.lens (\AssociateClientDeviceWithCoreDeviceErrorEntry' {message} -> message) (\s@AssociateClientDeviceWithCoreDeviceErrorEntry' {} a -> s {message = a} :: AssociateClientDeviceWithCoreDeviceErrorEntry)

-- | The name of the IoT thing whose associate request failed.
associateClientDeviceWithCoreDeviceErrorEntry_thingName :: Lens.Lens' AssociateClientDeviceWithCoreDeviceErrorEntry (Prelude.Maybe Prelude.Text)
associateClientDeviceWithCoreDeviceErrorEntry_thingName = Lens.lens (\AssociateClientDeviceWithCoreDeviceErrorEntry' {thingName} -> thingName) (\s@AssociateClientDeviceWithCoreDeviceErrorEntry' {} a -> s {thingName = a} :: AssociateClientDeviceWithCoreDeviceErrorEntry)

instance
  Data.FromJSON
    AssociateClientDeviceWithCoreDeviceErrorEntry
  where
  parseJSON =
    Data.withObject
      "AssociateClientDeviceWithCoreDeviceErrorEntry"
      ( \x ->
          AssociateClientDeviceWithCoreDeviceErrorEntry'
            Prelude.<$> (x Data..:? "code")
              Prelude.<*> (x Data..:? "message")
              Prelude.<*> (x Data..:? "thingName")
      )

instance
  Prelude.Hashable
    AssociateClientDeviceWithCoreDeviceErrorEntry
  where
  hashWithSalt
    _salt
    AssociateClientDeviceWithCoreDeviceErrorEntry' {..} =
      _salt `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` thingName

instance
  Prelude.NFData
    AssociateClientDeviceWithCoreDeviceErrorEntry
  where
  rnf
    AssociateClientDeviceWithCoreDeviceErrorEntry' {..} =
      Prelude.rnf code
        `Prelude.seq` Prelude.rnf message
        `Prelude.seq` Prelude.rnf thingName
