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
-- Module      : Amazonka.DLM.Types.CrossRegionCopyRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.CrossRegionCopyRule where

import qualified Amazonka.Core as Core
import Amazonka.DLM.Types.CrossRegionCopyDeprecateRule
import Amazonka.DLM.Types.CrossRegionCopyRetainRule
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a rule for cross-Region snapshot copies.
--
-- /See:/ 'newCrossRegionCopyRule' smart constructor.
data CrossRegionCopyRule = CrossRegionCopyRule'
  { -- | The AMI deprecation rule for cross-Region AMI copies created by the
    -- rule.
    deprecateRule :: Prelude.Maybe CrossRegionCopyDeprecateRule,
    -- | Avoid using this parameter when creating new policies. Instead, use
    -- __Target__ to specify a target Region or a target Outpost for snapshot
    -- copies.
    --
    -- For policies created before the __Target__ parameter was introduced,
    -- this parameter indicates the target Region for snapshot copies.
    targetRegion :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to copy all user-defined tags from the source snapshot
    -- to the cross-Region snapshot copy.
    copyTags :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the KMS key to use for EBS encryption.
    -- If this parameter is not specified, the default KMS key for the account
    -- is used.
    cmkArn :: Prelude.Maybe Prelude.Text,
    -- | The retention rule that indicates how long snapshot copies are to be
    -- retained in the destination Region.
    retainRule :: Prelude.Maybe CrossRegionCopyRetainRule,
    -- | The target Region or the Amazon Resource Name (ARN) of the target
    -- Outpost for the snapshot copies.
    --
    -- Use this parameter instead of __TargetRegion__. Do not specify both.
    target :: Prelude.Maybe Prelude.Text,
    -- | To encrypt a copy of an unencrypted snapshot if encryption by default is
    -- not enabled, enable encryption using this parameter. Copies of encrypted
    -- snapshots are encrypted, even if this parameter is false or if
    -- encryption by default is not enabled.
    encrypted :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrossRegionCopyRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deprecateRule', 'crossRegionCopyRule_deprecateRule' - The AMI deprecation rule for cross-Region AMI copies created by the
-- rule.
--
-- 'targetRegion', 'crossRegionCopyRule_targetRegion' - Avoid using this parameter when creating new policies. Instead, use
-- __Target__ to specify a target Region or a target Outpost for snapshot
-- copies.
--
-- For policies created before the __Target__ parameter was introduced,
-- this parameter indicates the target Region for snapshot copies.
--
-- 'copyTags', 'crossRegionCopyRule_copyTags' - Indicates whether to copy all user-defined tags from the source snapshot
-- to the cross-Region snapshot copy.
--
-- 'cmkArn', 'crossRegionCopyRule_cmkArn' - The Amazon Resource Name (ARN) of the KMS key to use for EBS encryption.
-- If this parameter is not specified, the default KMS key for the account
-- is used.
--
-- 'retainRule', 'crossRegionCopyRule_retainRule' - The retention rule that indicates how long snapshot copies are to be
-- retained in the destination Region.
--
-- 'target', 'crossRegionCopyRule_target' - The target Region or the Amazon Resource Name (ARN) of the target
-- Outpost for the snapshot copies.
--
-- Use this parameter instead of __TargetRegion__. Do not specify both.
--
-- 'encrypted', 'crossRegionCopyRule_encrypted' - To encrypt a copy of an unencrypted snapshot if encryption by default is
-- not enabled, enable encryption using this parameter. Copies of encrypted
-- snapshots are encrypted, even if this parameter is false or if
-- encryption by default is not enabled.
newCrossRegionCopyRule ::
  -- | 'encrypted'
  Prelude.Bool ->
  CrossRegionCopyRule
newCrossRegionCopyRule pEncrypted_ =
  CrossRegionCopyRule'
    { deprecateRule =
        Prelude.Nothing,
      targetRegion = Prelude.Nothing,
      copyTags = Prelude.Nothing,
      cmkArn = Prelude.Nothing,
      retainRule = Prelude.Nothing,
      target = Prelude.Nothing,
      encrypted = pEncrypted_
    }

-- | The AMI deprecation rule for cross-Region AMI copies created by the
-- rule.
crossRegionCopyRule_deprecateRule :: Lens.Lens' CrossRegionCopyRule (Prelude.Maybe CrossRegionCopyDeprecateRule)
crossRegionCopyRule_deprecateRule = Lens.lens (\CrossRegionCopyRule' {deprecateRule} -> deprecateRule) (\s@CrossRegionCopyRule' {} a -> s {deprecateRule = a} :: CrossRegionCopyRule)

-- | Avoid using this parameter when creating new policies. Instead, use
-- __Target__ to specify a target Region or a target Outpost for snapshot
-- copies.
--
-- For policies created before the __Target__ parameter was introduced,
-- this parameter indicates the target Region for snapshot copies.
crossRegionCopyRule_targetRegion :: Lens.Lens' CrossRegionCopyRule (Prelude.Maybe Prelude.Text)
crossRegionCopyRule_targetRegion = Lens.lens (\CrossRegionCopyRule' {targetRegion} -> targetRegion) (\s@CrossRegionCopyRule' {} a -> s {targetRegion = a} :: CrossRegionCopyRule)

-- | Indicates whether to copy all user-defined tags from the source snapshot
-- to the cross-Region snapshot copy.
crossRegionCopyRule_copyTags :: Lens.Lens' CrossRegionCopyRule (Prelude.Maybe Prelude.Bool)
crossRegionCopyRule_copyTags = Lens.lens (\CrossRegionCopyRule' {copyTags} -> copyTags) (\s@CrossRegionCopyRule' {} a -> s {copyTags = a} :: CrossRegionCopyRule)

-- | The Amazon Resource Name (ARN) of the KMS key to use for EBS encryption.
-- If this parameter is not specified, the default KMS key for the account
-- is used.
crossRegionCopyRule_cmkArn :: Lens.Lens' CrossRegionCopyRule (Prelude.Maybe Prelude.Text)
crossRegionCopyRule_cmkArn = Lens.lens (\CrossRegionCopyRule' {cmkArn} -> cmkArn) (\s@CrossRegionCopyRule' {} a -> s {cmkArn = a} :: CrossRegionCopyRule)

-- | The retention rule that indicates how long snapshot copies are to be
-- retained in the destination Region.
crossRegionCopyRule_retainRule :: Lens.Lens' CrossRegionCopyRule (Prelude.Maybe CrossRegionCopyRetainRule)
crossRegionCopyRule_retainRule = Lens.lens (\CrossRegionCopyRule' {retainRule} -> retainRule) (\s@CrossRegionCopyRule' {} a -> s {retainRule = a} :: CrossRegionCopyRule)

-- | The target Region or the Amazon Resource Name (ARN) of the target
-- Outpost for the snapshot copies.
--
-- Use this parameter instead of __TargetRegion__. Do not specify both.
crossRegionCopyRule_target :: Lens.Lens' CrossRegionCopyRule (Prelude.Maybe Prelude.Text)
crossRegionCopyRule_target = Lens.lens (\CrossRegionCopyRule' {target} -> target) (\s@CrossRegionCopyRule' {} a -> s {target = a} :: CrossRegionCopyRule)

-- | To encrypt a copy of an unencrypted snapshot if encryption by default is
-- not enabled, enable encryption using this parameter. Copies of encrypted
-- snapshots are encrypted, even if this parameter is false or if
-- encryption by default is not enabled.
crossRegionCopyRule_encrypted :: Lens.Lens' CrossRegionCopyRule Prelude.Bool
crossRegionCopyRule_encrypted = Lens.lens (\CrossRegionCopyRule' {encrypted} -> encrypted) (\s@CrossRegionCopyRule' {} a -> s {encrypted = a} :: CrossRegionCopyRule)

instance Core.FromJSON CrossRegionCopyRule where
  parseJSON =
    Core.withObject
      "CrossRegionCopyRule"
      ( \x ->
          CrossRegionCopyRule'
            Prelude.<$> (x Core..:? "DeprecateRule")
            Prelude.<*> (x Core..:? "TargetRegion")
            Prelude.<*> (x Core..:? "CopyTags")
            Prelude.<*> (x Core..:? "CmkArn")
            Prelude.<*> (x Core..:? "RetainRule")
            Prelude.<*> (x Core..:? "Target")
            Prelude.<*> (x Core..: "Encrypted")
      )

instance Prelude.Hashable CrossRegionCopyRule where
  hashWithSalt salt' CrossRegionCopyRule' {..} =
    salt' `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` retainRule
      `Prelude.hashWithSalt` cmkArn
      `Prelude.hashWithSalt` copyTags
      `Prelude.hashWithSalt` targetRegion
      `Prelude.hashWithSalt` deprecateRule

instance Prelude.NFData CrossRegionCopyRule where
  rnf CrossRegionCopyRule' {..} =
    Prelude.rnf deprecateRule
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf retainRule
      `Prelude.seq` Prelude.rnf cmkArn
      `Prelude.seq` Prelude.rnf copyTags
      `Prelude.seq` Prelude.rnf targetRegion

instance Core.ToJSON CrossRegionCopyRule where
  toJSON CrossRegionCopyRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeprecateRule" Core..=) Prelude.<$> deprecateRule,
            ("TargetRegion" Core..=) Prelude.<$> targetRegion,
            ("CopyTags" Core..=) Prelude.<$> copyTags,
            ("CmkArn" Core..=) Prelude.<$> cmkArn,
            ("RetainRule" Core..=) Prelude.<$> retainRule,
            ("Target" Core..=) Prelude.<$> target,
            Prelude.Just ("Encrypted" Core..= encrypted)
          ]
      )
