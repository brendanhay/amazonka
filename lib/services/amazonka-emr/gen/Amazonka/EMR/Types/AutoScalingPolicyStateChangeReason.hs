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
-- Module      : Amazonka.EMR.Types.AutoScalingPolicyStateChangeReason
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.AutoScalingPolicyStateChangeReason where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types.AutoScalingPolicyStateChangeReasonCode
import qualified Amazonka.Prelude as Prelude

-- | The reason for an AutoScalingPolicyStatus change.
--
-- /See:/ 'newAutoScalingPolicyStateChangeReason' smart constructor.
data AutoScalingPolicyStateChangeReason = AutoScalingPolicyStateChangeReason'
  { -- | The code indicating the reason for the change in status.@USER_REQUEST@
    -- indicates that the scaling policy status was changed by a user.
    -- @PROVISION_FAILURE@ indicates that the status change was because the
    -- policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
    code :: Prelude.Maybe AutoScalingPolicyStateChangeReasonCode,
    -- | A friendly, more verbose message that accompanies an automatic scaling
    -- policy state change.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingPolicyStateChangeReason' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'autoScalingPolicyStateChangeReason_code' - The code indicating the reason for the change in status.@USER_REQUEST@
-- indicates that the scaling policy status was changed by a user.
-- @PROVISION_FAILURE@ indicates that the status change was because the
-- policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
--
-- 'message', 'autoScalingPolicyStateChangeReason_message' - A friendly, more verbose message that accompanies an automatic scaling
-- policy state change.
newAutoScalingPolicyStateChangeReason ::
  AutoScalingPolicyStateChangeReason
newAutoScalingPolicyStateChangeReason =
  AutoScalingPolicyStateChangeReason'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The code indicating the reason for the change in status.@USER_REQUEST@
-- indicates that the scaling policy status was changed by a user.
-- @PROVISION_FAILURE@ indicates that the status change was because the
-- policy failed to provision. @CLEANUP_FAILURE@ indicates an error.
autoScalingPolicyStateChangeReason_code :: Lens.Lens' AutoScalingPolicyStateChangeReason (Prelude.Maybe AutoScalingPolicyStateChangeReasonCode)
autoScalingPolicyStateChangeReason_code = Lens.lens (\AutoScalingPolicyStateChangeReason' {code} -> code) (\s@AutoScalingPolicyStateChangeReason' {} a -> s {code = a} :: AutoScalingPolicyStateChangeReason)

-- | A friendly, more verbose message that accompanies an automatic scaling
-- policy state change.
autoScalingPolicyStateChangeReason_message :: Lens.Lens' AutoScalingPolicyStateChangeReason (Prelude.Maybe Prelude.Text)
autoScalingPolicyStateChangeReason_message = Lens.lens (\AutoScalingPolicyStateChangeReason' {message} -> message) (\s@AutoScalingPolicyStateChangeReason' {} a -> s {message = a} :: AutoScalingPolicyStateChangeReason)

instance
  Data.FromJSON
    AutoScalingPolicyStateChangeReason
  where
  parseJSON =
    Data.withObject
      "AutoScalingPolicyStateChangeReason"
      ( \x ->
          AutoScalingPolicyStateChangeReason'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Message")
      )

instance
  Prelude.Hashable
    AutoScalingPolicyStateChangeReason
  where
  hashWithSalt
    _salt
    AutoScalingPolicyStateChangeReason' {..} =
      _salt
        `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    AutoScalingPolicyStateChangeReason
  where
  rnf AutoScalingPolicyStateChangeReason' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
