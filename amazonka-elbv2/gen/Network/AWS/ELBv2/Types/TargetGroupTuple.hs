{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about how traffic will be distributed between multiple
-- target groups in a forward rule.
--
-- /See:/ 'newTargetGroupTuple' smart constructor.
data TargetGroupTuple = TargetGroupTuple'
  { -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The weight. The range is 0 to 999.
    weight :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { targetGroupArn = Prelude.Nothing,
      weight = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the target group.
targetGroupTuple_targetGroupArn :: Lens.Lens' TargetGroupTuple (Prelude.Maybe Prelude.Text)
targetGroupTuple_targetGroupArn = Lens.lens (\TargetGroupTuple' {targetGroupArn} -> targetGroupArn) (\s@TargetGroupTuple' {} a -> s {targetGroupArn = a} :: TargetGroupTuple)

-- | The weight. The range is 0 to 999.
targetGroupTuple_weight :: Lens.Lens' TargetGroupTuple (Prelude.Maybe Prelude.Int)
targetGroupTuple_weight = Lens.lens (\TargetGroupTuple' {weight} -> weight) (\s@TargetGroupTuple' {} a -> s {weight = a} :: TargetGroupTuple)

instance Prelude.FromXML TargetGroupTuple where
  parseXML x =
    TargetGroupTuple'
      Prelude.<$> (x Prelude..@? "TargetGroupArn")
      Prelude.<*> (x Prelude..@? "Weight")

instance Prelude.Hashable TargetGroupTuple

instance Prelude.NFData TargetGroupTuple

instance Prelude.ToQuery TargetGroupTuple where
  toQuery TargetGroupTuple' {..} =
    Prelude.mconcat
      [ "TargetGroupArn" Prelude.=: targetGroupArn,
        "Weight" Prelude.=: weight
      ]
