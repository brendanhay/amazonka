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
-- Module      : Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReason where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types.AutoScalingPolicyStateChangeReasonCode
import qualified Network.AWS.Lens as Lens

-- | The reason for an AutoScalingPolicyStatus change.
--
-- /See:/ 'newAutoScalingPolicyStateChangeReason' smart constructor.
data AutoScalingPolicyStateChangeReason = AutoScalingPolicyStateChangeReason'
  { -- | A friendly, more verbose message that accompanies an automatic scaling
    -- policy state change.
    message :: Core.Maybe Core.Text,
    -- | The code indicating the reason for the change in status.@USER_REQUEST@
    -- indicates that the scaling policy status was changed by a user.
    -- @PROVISION_FAILURE@ indicates that the status change was because the
    -- policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
    code :: Core.Maybe AutoScalingPolicyStateChangeReasonCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoScalingPolicyStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'autoScalingPolicyStateChangeReason_message' - A friendly, more verbose message that accompanies an automatic scaling
-- policy state change.
--
-- 'code', 'autoScalingPolicyStateChangeReason_code' - The code indicating the reason for the change in status.@USER_REQUEST@
-- indicates that the scaling policy status was changed by a user.
-- @PROVISION_FAILURE@ indicates that the status change was because the
-- policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
newAutoScalingPolicyStateChangeReason ::
  AutoScalingPolicyStateChangeReason
newAutoScalingPolicyStateChangeReason =
  AutoScalingPolicyStateChangeReason'
    { message =
        Core.Nothing,
      code = Core.Nothing
    }

-- | A friendly, more verbose message that accompanies an automatic scaling
-- policy state change.
autoScalingPolicyStateChangeReason_message :: Lens.Lens' AutoScalingPolicyStateChangeReason (Core.Maybe Core.Text)
autoScalingPolicyStateChangeReason_message = Lens.lens (\AutoScalingPolicyStateChangeReason' {message} -> message) (\s@AutoScalingPolicyStateChangeReason' {} a -> s {message = a} :: AutoScalingPolicyStateChangeReason)

-- | The code indicating the reason for the change in status.@USER_REQUEST@
-- indicates that the scaling policy status was changed by a user.
-- @PROVISION_FAILURE@ indicates that the status change was because the
-- policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
autoScalingPolicyStateChangeReason_code :: Lens.Lens' AutoScalingPolicyStateChangeReason (Core.Maybe AutoScalingPolicyStateChangeReasonCode)
autoScalingPolicyStateChangeReason_code = Lens.lens (\AutoScalingPolicyStateChangeReason' {code} -> code) (\s@AutoScalingPolicyStateChangeReason' {} a -> s {code = a} :: AutoScalingPolicyStateChangeReason)

instance
  Core.FromJSON
    AutoScalingPolicyStateChangeReason
  where
  parseJSON =
    Core.withObject
      "AutoScalingPolicyStateChangeReason"
      ( \x ->
          AutoScalingPolicyStateChangeReason'
            Core.<$> (x Core..:? "Message") Core.<*> (x Core..:? "Code")
      )

instance
  Core.Hashable
    AutoScalingPolicyStateChangeReason

instance
  Core.NFData
    AutoScalingPolicyStateChangeReason
