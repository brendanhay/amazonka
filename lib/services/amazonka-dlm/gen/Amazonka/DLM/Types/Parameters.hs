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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.Parameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DLM.Types.Tag
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | __[Snapshot and AMI policies only]__ Specifies optional parameters for
-- snapshot and AMI policies. The set of valid parameters depends on the
-- combination of policy type and target resource type.
--
-- If you choose to exclude boot volumes and you specify tags that
-- consequently exclude all of the additional data volumes attached to an
-- instance, then Amazon Data Lifecycle Manager will not create any
-- snapshots for the affected instance, and it will emit a
-- @SnapshotsCreateFailed@ Amazon CloudWatch metric. For more information,
-- see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitor-dlm-cw-metrics.html Monitor your policies using Amazon CloudWatch>.
--
-- /See:/ 'newParameters' smart constructor.
data Parameters = Parameters'
  { -- | __[Snapshot policies that target instances only]__ Indicates whether to
    -- exclude the root volume from multi-volume snapshot sets. The default is
    -- @false@. If you specify @true@, then the root volumes attached to
    -- targeted instances will be excluded from the multi-volume snapshot sets
    -- created by the policy.
    excludeBootVolume :: Prelude.Maybe Prelude.Bool,
    -- | __[Snapshot policies that target instances only]__ The tags used to
    -- identify data (non-root) volumes to exclude from multi-volume snapshot
    -- sets.
    --
    -- If you create a snapshot lifecycle policy that targets instances and you
    -- specify tags for this parameter, then data volumes with the specified
    -- tags that are attached to targeted instances will be excluded from the
    -- multi-volume snapshot sets created by the policy.
    excludeDataVolumeTags :: Prelude.Maybe [Tag],
    -- | __[AMI policies only]__ Indicates whether targeted instances are
    -- rebooted when the lifecycle policy runs. @true@ indicates that targeted
    -- instances are not rebooted when the policy runs. @false@ indicates that
    -- target instances are rebooted when the policy runs. The default is
    -- @true@ (instances are not rebooted).
    noReboot :: Prelude.Maybe Prelude.Bool
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
-- 'excludeBootVolume', 'parameters_excludeBootVolume' - __[Snapshot policies that target instances only]__ Indicates whether to
-- exclude the root volume from multi-volume snapshot sets. The default is
-- @false@. If you specify @true@, then the root volumes attached to
-- targeted instances will be excluded from the multi-volume snapshot sets
-- created by the policy.
--
-- 'excludeDataVolumeTags', 'parameters_excludeDataVolumeTags' - __[Snapshot policies that target instances only]__ The tags used to
-- identify data (non-root) volumes to exclude from multi-volume snapshot
-- sets.
--
-- If you create a snapshot lifecycle policy that targets instances and you
-- specify tags for this parameter, then data volumes with the specified
-- tags that are attached to targeted instances will be excluded from the
-- multi-volume snapshot sets created by the policy.
--
-- 'noReboot', 'parameters_noReboot' - __[AMI policies only]__ Indicates whether targeted instances are
-- rebooted when the lifecycle policy runs. @true@ indicates that targeted
-- instances are not rebooted when the policy runs. @false@ indicates that
-- target instances are rebooted when the policy runs. The default is
-- @true@ (instances are not rebooted).
newParameters ::
  Parameters
newParameters =
  Parameters'
    { excludeBootVolume = Prelude.Nothing,
      excludeDataVolumeTags = Prelude.Nothing,
      noReboot = Prelude.Nothing
    }

-- | __[Snapshot policies that target instances only]__ Indicates whether to
-- exclude the root volume from multi-volume snapshot sets. The default is
-- @false@. If you specify @true@, then the root volumes attached to
-- targeted instances will be excluded from the multi-volume snapshot sets
-- created by the policy.
parameters_excludeBootVolume :: Lens.Lens' Parameters (Prelude.Maybe Prelude.Bool)
parameters_excludeBootVolume = Lens.lens (\Parameters' {excludeBootVolume} -> excludeBootVolume) (\s@Parameters' {} a -> s {excludeBootVolume = a} :: Parameters)

-- | __[Snapshot policies that target instances only]__ The tags used to
-- identify data (non-root) volumes to exclude from multi-volume snapshot
-- sets.
--
-- If you create a snapshot lifecycle policy that targets instances and you
-- specify tags for this parameter, then data volumes with the specified
-- tags that are attached to targeted instances will be excluded from the
-- multi-volume snapshot sets created by the policy.
parameters_excludeDataVolumeTags :: Lens.Lens' Parameters (Prelude.Maybe [Tag])
parameters_excludeDataVolumeTags = Lens.lens (\Parameters' {excludeDataVolumeTags} -> excludeDataVolumeTags) (\s@Parameters' {} a -> s {excludeDataVolumeTags = a} :: Parameters) Prelude.. Lens.mapping Lens.coerced

-- | __[AMI policies only]__ Indicates whether targeted instances are
-- rebooted when the lifecycle policy runs. @true@ indicates that targeted
-- instances are not rebooted when the policy runs. @false@ indicates that
-- target instances are rebooted when the policy runs. The default is
-- @true@ (instances are not rebooted).
parameters_noReboot :: Lens.Lens' Parameters (Prelude.Maybe Prelude.Bool)
parameters_noReboot = Lens.lens (\Parameters' {noReboot} -> noReboot) (\s@Parameters' {} a -> s {noReboot = a} :: Parameters)

instance Data.FromJSON Parameters where
  parseJSON =
    Data.withObject
      "Parameters"
      ( \x ->
          Parameters'
            Prelude.<$> (x Data..:? "ExcludeBootVolume")
            Prelude.<*> ( x Data..:? "ExcludeDataVolumeTags"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "NoReboot")
      )

instance Prelude.Hashable Parameters where
  hashWithSalt _salt Parameters' {..} =
    _salt `Prelude.hashWithSalt` excludeBootVolume
      `Prelude.hashWithSalt` excludeDataVolumeTags
      `Prelude.hashWithSalt` noReboot

instance Prelude.NFData Parameters where
  rnf Parameters' {..} =
    Prelude.rnf excludeBootVolume
      `Prelude.seq` Prelude.rnf excludeDataVolumeTags
      `Prelude.seq` Prelude.rnf noReboot

instance Data.ToJSON Parameters where
  toJSON Parameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExcludeBootVolume" Data..=)
              Prelude.<$> excludeBootVolume,
            ("ExcludeDataVolumeTags" Data..=)
              Prelude.<$> excludeDataVolumeTags,
            ("NoReboot" Data..=) Prelude.<$> noReboot
          ]
      )
