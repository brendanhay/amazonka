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
-- Module      : Network.AWS.EMR.Types.InstanceGroupStateChangeReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceGroupStateChangeReason where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.InstanceGroupStateChangeReasonCode
import qualified Network.AWS.Lens as Lens

-- | The status change reason details for the instance group.
--
-- /See:/ 'newInstanceGroupStateChangeReason' smart constructor.
data InstanceGroupStateChangeReason = InstanceGroupStateChangeReason'
  { -- | The status change reason description.
    message :: Core.Maybe Core.Text,
    -- | The programmable code for the state change reason.
    code :: Core.Maybe InstanceGroupStateChangeReasonCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      code = Core.Nothing
    }

-- | The status change reason description.
instanceGroupStateChangeReason_message :: Lens.Lens' InstanceGroupStateChangeReason (Core.Maybe Core.Text)
instanceGroupStateChangeReason_message = Lens.lens (\InstanceGroupStateChangeReason' {message} -> message) (\s@InstanceGroupStateChangeReason' {} a -> s {message = a} :: InstanceGroupStateChangeReason)

-- | The programmable code for the state change reason.
instanceGroupStateChangeReason_code :: Lens.Lens' InstanceGroupStateChangeReason (Core.Maybe InstanceGroupStateChangeReasonCode)
instanceGroupStateChangeReason_code = Lens.lens (\InstanceGroupStateChangeReason' {code} -> code) (\s@InstanceGroupStateChangeReason' {} a -> s {code = a} :: InstanceGroupStateChangeReason)

instance Core.FromJSON InstanceGroupStateChangeReason where
  parseJSON =
    Core.withObject
      "InstanceGroupStateChangeReason"
      ( \x ->
          InstanceGroupStateChangeReason'
            Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "Code")
      )

instance Core.Hashable InstanceGroupStateChangeReason

instance Core.NFData InstanceGroupStateChangeReason
