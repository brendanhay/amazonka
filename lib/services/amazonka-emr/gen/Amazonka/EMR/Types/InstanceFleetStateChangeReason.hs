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
-- Module      : Amazonka.EMR.Types.InstanceFleetStateChangeReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceFleetStateChangeReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.InstanceFleetStateChangeReasonCode
import qualified Amazonka.Prelude as Prelude

-- | Provides status change reason details for the instance fleet.
--
-- The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- /See:/ 'newInstanceFleetStateChangeReason' smart constructor.
data InstanceFleetStateChangeReason = InstanceFleetStateChangeReason'
  { -- | A code corresponding to the reason the state change occurred.
    code :: Prelude.Maybe InstanceFleetStateChangeReasonCode,
    -- | An explanatory message.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceFleetStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'instanceFleetStateChangeReason_code' - A code corresponding to the reason the state change occurred.
--
-- 'message', 'instanceFleetStateChangeReason_message' - An explanatory message.
newInstanceFleetStateChangeReason ::
  InstanceFleetStateChangeReason
newInstanceFleetStateChangeReason =
  InstanceFleetStateChangeReason'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | A code corresponding to the reason the state change occurred.
instanceFleetStateChangeReason_code :: Lens.Lens' InstanceFleetStateChangeReason (Prelude.Maybe InstanceFleetStateChangeReasonCode)
instanceFleetStateChangeReason_code = Lens.lens (\InstanceFleetStateChangeReason' {code} -> code) (\s@InstanceFleetStateChangeReason' {} a -> s {code = a} :: InstanceFleetStateChangeReason)

-- | An explanatory message.
instanceFleetStateChangeReason_message :: Lens.Lens' InstanceFleetStateChangeReason (Prelude.Maybe Prelude.Text)
instanceFleetStateChangeReason_message = Lens.lens (\InstanceFleetStateChangeReason' {message} -> message) (\s@InstanceFleetStateChangeReason' {} a -> s {message = a} :: InstanceFleetStateChangeReason)

instance Data.FromJSON InstanceFleetStateChangeReason where
  parseJSON =
    Data.withObject
      "InstanceFleetStateChangeReason"
      ( \x ->
          InstanceFleetStateChangeReason'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance
  Prelude.Hashable
    InstanceFleetStateChangeReason
  where
  hashWithSalt
    _salt
    InstanceFleetStateChangeReason' {..} =
      _salt `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    InstanceFleetStateChangeReason
  where
  rnf InstanceFleetStateChangeReason' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
