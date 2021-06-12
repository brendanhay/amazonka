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
-- Module      : Network.AWS.ELBv2.Types.TargetGroupTuple
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetGroupTuple where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about how traffic will be distributed between multiple
-- target groups in a forward rule.
--
-- /See:/ 'newTargetGroupTuple' smart constructor.
data TargetGroupTuple = TargetGroupTuple'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Core.Maybe Core.Text,
    -- | The weight. The range is 0 to 999.
    weight :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TargetGroupTuple' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupArn', 'targetGroupTuple_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
--
-- 'weight', 'targetGroupTuple_weight' - The weight. The range is 0 to 999.
newTargetGroupTuple ::
  TargetGroupTuple
newTargetGroupTuple =
  TargetGroupTuple'
    { targetGroupArn = Core.Nothing,
      weight = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the target group.
targetGroupTuple_targetGroupArn :: Lens.Lens' TargetGroupTuple (Core.Maybe Core.Text)
targetGroupTuple_targetGroupArn = Lens.lens (\TargetGroupTuple' {targetGroupArn} -> targetGroupArn) (\s@TargetGroupTuple' {} a -> s {targetGroupArn = a} :: TargetGroupTuple)

-- | The weight. The range is 0 to 999.
targetGroupTuple_weight :: Lens.Lens' TargetGroupTuple (Core.Maybe Core.Int)
targetGroupTuple_weight = Lens.lens (\TargetGroupTuple' {weight} -> weight) (\s@TargetGroupTuple' {} a -> s {weight = a} :: TargetGroupTuple)

instance Core.FromXML TargetGroupTuple where
  parseXML x =
    TargetGroupTuple'
      Core.<$> (x Core..@? "TargetGroupArn")
      Core.<*> (x Core..@? "Weight")

instance Core.Hashable TargetGroupTuple

instance Core.NFData TargetGroupTuple

instance Core.ToQuery TargetGroupTuple where
  toQuery TargetGroupTuple' {..} =
    Core.mconcat
      [ "TargetGroupArn" Core.=: targetGroupArn,
        "Weight" Core.=: weight
      ]
