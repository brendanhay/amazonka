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
-- Module      : Amazonka.GreengrassV2.Types.DisassociateClientDeviceFromCoreDeviceErrorEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.DisassociateClientDeviceFromCoreDeviceErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an error that occurs from a request to disassociate a client
-- device from a core device. The
-- <https://docs.aws.amazon.com/greengrass/v2/APIReference/API_BatchDisassociateClientDeviceWithCoreDevice.html BatchDisassociateClientDeviceWithCoreDevice>
-- operation returns a list of these errors.
--
-- /See:/ 'newDisassociateClientDeviceFromCoreDeviceErrorEntry' smart constructor.
data DisassociateClientDeviceFromCoreDeviceErrorEntry = DisassociateClientDeviceFromCoreDeviceErrorEntry'
  { -- | A message that provides additional information about the error.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the IoT thing whose disassociate request failed.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The error code for the request.
    code :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateClientDeviceFromCoreDeviceErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'disassociateClientDeviceFromCoreDeviceErrorEntry_message' - A message that provides additional information about the error.
--
-- 'thingName', 'disassociateClientDeviceFromCoreDeviceErrorEntry_thingName' - The name of the IoT thing whose disassociate request failed.
--
-- 'code', 'disassociateClientDeviceFromCoreDeviceErrorEntry_code' - The error code for the request.
newDisassociateClientDeviceFromCoreDeviceErrorEntry ::
  DisassociateClientDeviceFromCoreDeviceErrorEntry
newDisassociateClientDeviceFromCoreDeviceErrorEntry =
  DisassociateClientDeviceFromCoreDeviceErrorEntry'
    { message =
        Prelude.Nothing,
      thingName =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | A message that provides additional information about the error.
disassociateClientDeviceFromCoreDeviceErrorEntry_message :: Lens.Lens' DisassociateClientDeviceFromCoreDeviceErrorEntry (Prelude.Maybe Prelude.Text)
disassociateClientDeviceFromCoreDeviceErrorEntry_message = Lens.lens (\DisassociateClientDeviceFromCoreDeviceErrorEntry' {message} -> message) (\s@DisassociateClientDeviceFromCoreDeviceErrorEntry' {} a -> s {message = a} :: DisassociateClientDeviceFromCoreDeviceErrorEntry)

-- | The name of the IoT thing whose disassociate request failed.
disassociateClientDeviceFromCoreDeviceErrorEntry_thingName :: Lens.Lens' DisassociateClientDeviceFromCoreDeviceErrorEntry (Prelude.Maybe Prelude.Text)
disassociateClientDeviceFromCoreDeviceErrorEntry_thingName = Lens.lens (\DisassociateClientDeviceFromCoreDeviceErrorEntry' {thingName} -> thingName) (\s@DisassociateClientDeviceFromCoreDeviceErrorEntry' {} a -> s {thingName = a} :: DisassociateClientDeviceFromCoreDeviceErrorEntry)

-- | The error code for the request.
disassociateClientDeviceFromCoreDeviceErrorEntry_code :: Lens.Lens' DisassociateClientDeviceFromCoreDeviceErrorEntry (Prelude.Maybe Prelude.Text)
disassociateClientDeviceFromCoreDeviceErrorEntry_code = Lens.lens (\DisassociateClientDeviceFromCoreDeviceErrorEntry' {code} -> code) (\s@DisassociateClientDeviceFromCoreDeviceErrorEntry' {} a -> s {code = a} :: DisassociateClientDeviceFromCoreDeviceErrorEntry)

instance
  Data.FromJSON
    DisassociateClientDeviceFromCoreDeviceErrorEntry
  where
  parseJSON =
    Data.withObject
      "DisassociateClientDeviceFromCoreDeviceErrorEntry"
      ( \x ->
          DisassociateClientDeviceFromCoreDeviceErrorEntry'
            Prelude.<$> (x Data..:? "message")
              Prelude.<*> (x Data..:? "thingName")
              Prelude.<*> (x Data..:? "code")
      )

instance
  Prelude.Hashable
    DisassociateClientDeviceFromCoreDeviceErrorEntry
  where
  hashWithSalt
    _salt
    DisassociateClientDeviceFromCoreDeviceErrorEntry' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` thingName
        `Prelude.hashWithSalt` code

instance
  Prelude.NFData
    DisassociateClientDeviceFromCoreDeviceErrorEntry
  where
  rnf
    DisassociateClientDeviceFromCoreDeviceErrorEntry' {..} =
      Prelude.rnf message
        `Prelude.seq` Prelude.rnf thingName
        `Prelude.seq` Prelude.rnf code
