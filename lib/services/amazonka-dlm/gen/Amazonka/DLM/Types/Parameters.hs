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
-- Module      : Amazonka.DLM.Types.Parameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.Parameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies optional parameters to add to a policy. The set of valid
-- parameters depends on the combination of policy type and resource type.
--
-- /See:/ 'newParameters' smart constructor.
data Parameters = Parameters'
  { -- | Applies to AMI lifecycle policies only. Indicates whether targeted
    -- instances are rebooted when the lifecycle policy runs. @true@ indicates
    -- that targeted instances are not rebooted when the policy runs. @false@
    -- indicates that target instances are rebooted when the policy runs. The
    -- default is @true@ (instances are not rebooted).
    noReboot :: Prelude.Maybe Prelude.Bool,
    -- | [EBS Snapshot Management – Instance policies only] Indicates whether to
    -- exclude the root volume from snapshots created using
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSnapshots.html CreateSnapshots>.
    -- The default is false.
    excludeBootVolume :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Parameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'noReboot', 'parameters_noReboot' - Applies to AMI lifecycle policies only. Indicates whether targeted
-- instances are rebooted when the lifecycle policy runs. @true@ indicates
-- that targeted instances are not rebooted when the policy runs. @false@
-- indicates that target instances are rebooted when the policy runs. The
-- default is @true@ (instances are not rebooted).
--
-- 'excludeBootVolume', 'parameters_excludeBootVolume' - [EBS Snapshot Management – Instance policies only] Indicates whether to
-- exclude the root volume from snapshots created using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSnapshots.html CreateSnapshots>.
-- The default is false.
newParameters ::
  Parameters
newParameters =
  Parameters'
    { noReboot = Prelude.Nothing,
      excludeBootVolume = Prelude.Nothing
    }

-- | Applies to AMI lifecycle policies only. Indicates whether targeted
-- instances are rebooted when the lifecycle policy runs. @true@ indicates
-- that targeted instances are not rebooted when the policy runs. @false@
-- indicates that target instances are rebooted when the policy runs. The
-- default is @true@ (instances are not rebooted).
parameters_noReboot :: Lens.Lens' Parameters (Prelude.Maybe Prelude.Bool)
parameters_noReboot = Lens.lens (\Parameters' {noReboot} -> noReboot) (\s@Parameters' {} a -> s {noReboot = a} :: Parameters)

-- | [EBS Snapshot Management – Instance policies only] Indicates whether to
-- exclude the root volume from snapshots created using
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CreateSnapshots.html CreateSnapshots>.
-- The default is false.
parameters_excludeBootVolume :: Lens.Lens' Parameters (Prelude.Maybe Prelude.Bool)
parameters_excludeBootVolume = Lens.lens (\Parameters' {excludeBootVolume} -> excludeBootVolume) (\s@Parameters' {} a -> s {excludeBootVolume = a} :: Parameters)

instance Core.FromJSON Parameters where
  parseJSON =
    Core.withObject
      "Parameters"
      ( \x ->
          Parameters'
            Prelude.<$> (x Core..:? "NoReboot")
            Prelude.<*> (x Core..:? "ExcludeBootVolume")
      )

instance Prelude.Hashable Parameters where
  hashWithSalt _salt Parameters' {..} =
    _salt `Prelude.hashWithSalt` noReboot
      `Prelude.hashWithSalt` excludeBootVolume

instance Prelude.NFData Parameters where
  rnf Parameters' {..} =
    Prelude.rnf noReboot
      `Prelude.seq` Prelude.rnf excludeBootVolume

instance Core.ToJSON Parameters where
  toJSON Parameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NoReboot" Core..=) Prelude.<$> noReboot,
            ("ExcludeBootVolume" Core..=)
              Prelude.<$> excludeBootVolume
          ]
      )
