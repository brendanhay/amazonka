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
-- Module      : Network.AWS.CodeStarNotifications.Types.TargetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeStarNotifications.Types.TargetSummary where

import Network.AWS.CodeStarNotifications.Types.TargetStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about the targets specified for a notification rule.
--
-- /See:/ 'newTargetSummary' smart constructor.
data TargetSummary = TargetSummary'
  { -- | The type of the target (for example, SNS).
    targetType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the SNS topic.
    targetAddress :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The status of the target.
    targetStatus :: Prelude.Maybe TargetStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetType', 'targetSummary_targetType' - The type of the target (for example, SNS).
--
-- 'targetAddress', 'targetSummary_targetAddress' - The Amazon Resource Name (ARN) of the SNS topic.
--
-- 'targetStatus', 'targetSummary_targetStatus' - The status of the target.
newTargetSummary ::
  TargetSummary
newTargetSummary =
  TargetSummary'
    { targetType = Prelude.Nothing,
      targetAddress = Prelude.Nothing,
      targetStatus = Prelude.Nothing
    }

-- | The type of the target (for example, SNS).
targetSummary_targetType :: Lens.Lens' TargetSummary (Prelude.Maybe Prelude.Text)
targetSummary_targetType = Lens.lens (\TargetSummary' {targetType} -> targetType) (\s@TargetSummary' {} a -> s {targetType = a} :: TargetSummary)

-- | The Amazon Resource Name (ARN) of the SNS topic.
targetSummary_targetAddress :: Lens.Lens' TargetSummary (Prelude.Maybe Prelude.Text)
targetSummary_targetAddress = Lens.lens (\TargetSummary' {targetAddress} -> targetAddress) (\s@TargetSummary' {} a -> s {targetAddress = a} :: TargetSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The status of the target.
targetSummary_targetStatus :: Lens.Lens' TargetSummary (Prelude.Maybe TargetStatus)
targetSummary_targetStatus = Lens.lens (\TargetSummary' {targetStatus} -> targetStatus) (\s@TargetSummary' {} a -> s {targetStatus = a} :: TargetSummary)

instance Core.FromJSON TargetSummary where
  parseJSON =
    Core.withObject
      "TargetSummary"
      ( \x ->
          TargetSummary'
            Prelude.<$> (x Core..:? "TargetType")
            Prelude.<*> (x Core..:? "TargetAddress")
            Prelude.<*> (x Core..:? "TargetStatus")
      )

instance Prelude.Hashable TargetSummary

instance Prelude.NFData TargetSummary
