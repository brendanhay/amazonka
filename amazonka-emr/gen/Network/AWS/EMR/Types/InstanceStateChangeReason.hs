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
-- Module      : Network.AWS.EMR.Types.InstanceStateChangeReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceStateChangeReason where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.InstanceStateChangeReasonCode
import qualified Network.AWS.Lens as Lens

-- | The details of the status change reason for the instance.
--
-- /See:/ 'newInstanceStateChangeReason' smart constructor.
data InstanceStateChangeReason = InstanceStateChangeReason'
  { -- | The status change reason description.
    message :: Core.Maybe Core.Text,
    -- | The programmable code for the state change reason.
    code :: Core.Maybe InstanceStateChangeReasonCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { message = Core.Nothing,
      code = Core.Nothing
    }

-- | The status change reason description.
instanceStateChangeReason_message :: Lens.Lens' InstanceStateChangeReason (Core.Maybe Core.Text)
instanceStateChangeReason_message = Lens.lens (\InstanceStateChangeReason' {message} -> message) (\s@InstanceStateChangeReason' {} a -> s {message = a} :: InstanceStateChangeReason)

-- | The programmable code for the state change reason.
instanceStateChangeReason_code :: Lens.Lens' InstanceStateChangeReason (Core.Maybe InstanceStateChangeReasonCode)
instanceStateChangeReason_code = Lens.lens (\InstanceStateChangeReason' {code} -> code) (\s@InstanceStateChangeReason' {} a -> s {code = a} :: InstanceStateChangeReason)

instance Core.FromJSON InstanceStateChangeReason where
  parseJSON =
    Core.withObject
      "InstanceStateChangeReason"
      ( \x ->
          InstanceStateChangeReason'
            Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "Code")
      )

instance Core.Hashable InstanceStateChangeReason

instance Core.NFData InstanceStateChangeReason
