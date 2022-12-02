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
-- Module      : Amazonka.EMR.Types.InstanceStateChangeReason
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.InstanceStateChangeReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.InstanceStateChangeReasonCode
import qualified Amazonka.Prelude as Prelude

-- | The details of the status change reason for the instance.
--
-- /See:/ 'newInstanceStateChangeReason' smart constructor.
data InstanceStateChangeReason = InstanceStateChangeReason'
  { -- | The status change reason description.
    message :: Prelude.Maybe Prelude.Text,
    -- | The programmable code for the state change reason.
    code :: Prelude.Maybe InstanceStateChangeReasonCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'instanceStateChangeReason_message' - The status change reason description.
--
-- 'code', 'instanceStateChangeReason_code' - The programmable code for the state change reason.
newInstanceStateChangeReason ::
  InstanceStateChangeReason
newInstanceStateChangeReason =
  InstanceStateChangeReason'
    { message =
        Prelude.Nothing,
      code = Prelude.Nothing
    }

-- | The status change reason description.
instanceStateChangeReason_message :: Lens.Lens' InstanceStateChangeReason (Prelude.Maybe Prelude.Text)
instanceStateChangeReason_message = Lens.lens (\InstanceStateChangeReason' {message} -> message) (\s@InstanceStateChangeReason' {} a -> s {message = a} :: InstanceStateChangeReason)

-- | The programmable code for the state change reason.
instanceStateChangeReason_code :: Lens.Lens' InstanceStateChangeReason (Prelude.Maybe InstanceStateChangeReasonCode)
instanceStateChangeReason_code = Lens.lens (\InstanceStateChangeReason' {code} -> code) (\s@InstanceStateChangeReason' {} a -> s {code = a} :: InstanceStateChangeReason)

instance Data.FromJSON InstanceStateChangeReason where
  parseJSON =
    Data.withObject
      "InstanceStateChangeReason"
      ( \x ->
          InstanceStateChangeReason'
            Prelude.<$> (x Data..:? "Message")
            Prelude.<*> (x Data..:? "Code")
      )

instance Prelude.Hashable InstanceStateChangeReason where
  hashWithSalt _salt InstanceStateChangeReason' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` code

instance Prelude.NFData InstanceStateChangeReason where
  rnf InstanceStateChangeReason' {..} =
    Prelude.rnf message `Prelude.seq` Prelude.rnf code
