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
-- Module      : Amazonka.IoTEvents.Types.IotEventsInputIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.IotEventsInputIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The identifier of the input routed to AWS IoT Events.
--
-- /See:/ 'newIotEventsInputIdentifier' smart constructor.
data IotEventsInputIdentifier = IotEventsInputIdentifier'
  { -- | The name of the input routed to AWS IoT Events.
    inputName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IotEventsInputIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inputName', 'iotEventsInputIdentifier_inputName' - The name of the input routed to AWS IoT Events.
newIotEventsInputIdentifier ::
  -- | 'inputName'
  Prelude.Text ->
  IotEventsInputIdentifier
newIotEventsInputIdentifier pInputName_ =
  IotEventsInputIdentifier' {inputName = pInputName_}

-- | The name of the input routed to AWS IoT Events.
iotEventsInputIdentifier_inputName :: Lens.Lens' IotEventsInputIdentifier Prelude.Text
iotEventsInputIdentifier_inputName = Lens.lens (\IotEventsInputIdentifier' {inputName} -> inputName) (\s@IotEventsInputIdentifier' {} a -> s {inputName = a} :: IotEventsInputIdentifier)

instance Prelude.Hashable IotEventsInputIdentifier where
  hashWithSalt _salt IotEventsInputIdentifier' {..} =
    _salt `Prelude.hashWithSalt` inputName

instance Prelude.NFData IotEventsInputIdentifier where
  rnf IotEventsInputIdentifier' {..} =
    Prelude.rnf inputName

instance Core.ToJSON IotEventsInputIdentifier where
  toJSON IotEventsInputIdentifier' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("inputName" Core..= inputName)]
      )
