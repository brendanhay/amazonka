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
-- Module      : Amazonka.EMR.Types.InstanceGroupStateChangeReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceGroupStateChangeReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.InstanceGroupStateChangeReasonCode
import qualified Amazonka.Prelude as Prelude

-- | The status change reason details for the instance group.
--
-- /See:/ 'newInstanceGroupStateChangeReason' smart constructor.
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason'
  { -- | The programmable code for the state change reason.
    code :: Prelude.Maybe InstanceGroupStateChangeReasonCode,
    -- | The status change reason description.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceGroupStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'instanceGroupStateChangeReason_code' - The programmable code for the state change reason.
--
-- 'message', 'instanceGroupStateChangeReason_message' - The status change reason description.
newInstanceGroupStateChangeReason ::
  InstanceGroupStateChangeReason
newInstanceGroupStateChangeReason =
  InstanceGroupStateChangeReason'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The programmable code for the state change reason.
instanceGroupStateChangeReason_code :: Lens.Lens' InstanceGroupStateChangeReason (Prelude.Maybe InstanceGroupStateChangeReasonCode)
instanceGroupStateChangeReason_code = Lens.lens (\InstanceGroupStateChangeReason' {code} -> code) (\s@InstanceGroupStateChangeReason' {} a -> s {code = a} :: InstanceGroupStateChangeReason)

-- | The status change reason description.
instanceGroupStateChangeReason_message :: Lens.Lens' InstanceGroupStateChangeReason (Prelude.Maybe Prelude.Text)
instanceGroupStateChangeReason_message = Lens.lens (\InstanceGroupStateChangeReason' {message} -> message) (\s@InstanceGroupStateChangeReason' {} a -> s {message = a} :: InstanceGroupStateChangeReason)

instance Data.FromJSON InstanceGroupStateChangeReason where
  parseJSON =
    Data.withObject
      "InstanceGroupStateChangeReason"
      ( \x ->
          InstanceGroupStateChangeReason'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance
  Prelude.Hashable
    InstanceGroupStateChangeReason
  where
  hashWithSalt
    _salt
    InstanceGroupStateChangeReason' {..} =
      _salt `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    InstanceGroupStateChangeReason
  where
  rnf InstanceGroupStateChangeReason' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
