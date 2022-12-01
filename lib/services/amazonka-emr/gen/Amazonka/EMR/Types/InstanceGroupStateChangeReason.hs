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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceGroupStateChangeReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EMR.Types.InstanceGroupStateChangeReasonCode
import qualified Amazonka.Prelude as Prelude

-- | The status change reason details for the instance group.
--
-- /See:/ 'newInstanceGroupStateChangeReason' smart constructor.
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason'
  { -- | The status change reason description.
    message :: Prelude.Maybe Prelude.Text,
    -- | The programmable code for the state change reason.
    code :: Prelude.Maybe InstanceGroupStateChangeReasonCode
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
-- 'message', 'instanceGroupStateChangeReason_message' - The status change reason description.
--
-- 'code', 'instanceGroupStateChangeReason_code' - The programmable code for the state change reason.
newInstanceGroupStateChangeReason ::
  InstanceGroupStateChangeReason
newInstanceGroupStateChangeReason =
  InstanceGroupStateChangeReason'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The status change reason description.
instanceGroupStateChangeReason_message :: Lens.Lens' InstanceGroupStateChangeReason (Prelude.Maybe Prelude.Text)
instanceGroupStateChangeReason_message = Lens.lens (\InstanceGroupStateChangeReason' {message} -> message) (\s@InstanceGroupStateChangeReason' {} a -> s {message = a} :: InstanceGroupStateChangeReason)

-- | The programmable code for the state change reason.
instanceGroupStateChangeReason_code :: Lens.Lens' InstanceGroupStateChangeReason (Prelude.Maybe InstanceGroupStateChangeReasonCode)
instanceGroupStateChangeReason_code = Lens.lens (\InstanceGroupStateChangeReason' {code} -> code) (\s@InstanceGroupStateChangeReason' {} a -> s {code = a} :: InstanceGroupStateChangeReason)

instance Core.FromJSON InstanceGroupStateChangeReason where
  parseJSON =
    Core.withObject
      "InstanceGroupStateChangeReason"
      ( \x ->
          InstanceGroupStateChangeReason'
            Prelude.<$> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Code")
      )

instance
  Prelude.Hashable
    InstanceGroupStateChangeReason
  where
  hashWithSalt
    _salt
    InstanceGroupStateChangeReason' {..} =
      _salt `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` code

instance
  Prelude.NFData
    InstanceGroupStateChangeReason
  where
  rnf InstanceGroupStateChangeReason' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
