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
-- Module      : Network.AWS.EMR.Types.ScalingConstraints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ScalingConstraints where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The upper and lower EC2 instance limits for an automatic scaling policy.
-- Automatic scaling activities triggered by automatic scaling rules will
-- not cause an instance group to grow above or below these limits.
--
-- /See:/ 'newScalingConstraints' smart constructor.
data ScalingConstraints = ScalingConstraints'
  { -- | The lower boundary of EC2 instances in an instance group below which
    -- scaling activities are not allowed to shrink. Scale-in activities will
    -- not terminate instances below this boundary.
    minCapacity :: Prelude.Int,
    -- | The upper boundary of EC2 instances in an instance group beyond which
    -- scaling activities are not allowed to grow. Scale-out activities will
    -- not add instances beyond this boundary.
    maxCapacity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ScalingConstraints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minCapacity', 'scalingConstraints_minCapacity' - The lower boundary of EC2 instances in an instance group below which
-- scaling activities are not allowed to shrink. Scale-in activities will
-- not terminate instances below this boundary.
--
-- 'maxCapacity', 'scalingConstraints_maxCapacity' - The upper boundary of EC2 instances in an instance group beyond which
-- scaling activities are not allowed to grow. Scale-out activities will
-- not add instances beyond this boundary.
newScalingConstraints ::
  -- | 'minCapacity'
  Prelude.Int ->
  -- | 'maxCapacity'
  Prelude.Int ->
  ScalingConstraints
newScalingConstraints pMinCapacity_ pMaxCapacity_ =
  ScalingConstraints'
    { minCapacity = pMinCapacity_,
      maxCapacity = pMaxCapacity_
    }

-- | The lower boundary of EC2 instances in an instance group below which
-- scaling activities are not allowed to shrink. Scale-in activities will
-- not terminate instances below this boundary.
scalingConstraints_minCapacity :: Lens.Lens' ScalingConstraints Prelude.Int
scalingConstraints_minCapacity = Lens.lens (\ScalingConstraints' {minCapacity} -> minCapacity) (\s@ScalingConstraints' {} a -> s {minCapacity = a} :: ScalingConstraints)

-- | The upper boundary of EC2 instances in an instance group beyond which
-- scaling activities are not allowed to grow. Scale-out activities will
-- not add instances beyond this boundary.
scalingConstraints_maxCapacity :: Lens.Lens' ScalingConstraints Prelude.Int
scalingConstraints_maxCapacity = Lens.lens (\ScalingConstraints' {maxCapacity} -> maxCapacity) (\s@ScalingConstraints' {} a -> s {maxCapacity = a} :: ScalingConstraints)

instance Prelude.FromJSON ScalingConstraints where
  parseJSON =
    Prelude.withObject
      "ScalingConstraints"
      ( \x ->
          ScalingConstraints'
            Prelude.<$> (x Prelude..: "MinCapacity")
            Prelude.<*> (x Prelude..: "MaxCapacity")
      )

instance Prelude.Hashable ScalingConstraints

instance Prelude.NFData ScalingConstraints

instance Prelude.ToJSON ScalingConstraints where
  toJSON ScalingConstraints' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("MinCapacity" Prelude..= minCapacity),
            Prelude.Just ("MaxCapacity" Prelude..= maxCapacity)
          ]
      )
