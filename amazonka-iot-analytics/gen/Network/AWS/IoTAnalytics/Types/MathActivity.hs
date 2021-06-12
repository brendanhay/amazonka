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
-- Module      : Network.AWS.IoTAnalytics.Types.MathActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.MathActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An activity that computes an arithmetic expression using the message\'s
-- attributes.
--
-- /See:/ 'newMathActivity' smart constructor.
data MathActivity = MathActivity'
  { -- | The next activity in the pipeline.
    next :: Core.Maybe Core.Text,
    -- | The name of the math activity.
    name :: Core.Text,
    -- | The name of the attribute that contains the result of the math
    -- operation.
    attribute :: Core.Text,
    -- | An expression that uses one or more existing attributes and must return
    -- an integer value.
    math :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MathActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'next', 'mathActivity_next' - The next activity in the pipeline.
--
-- 'name', 'mathActivity_name' - The name of the math activity.
--
-- 'attribute', 'mathActivity_attribute' - The name of the attribute that contains the result of the math
-- operation.
--
-- 'math', 'mathActivity_math' - An expression that uses one or more existing attributes and must return
-- an integer value.
newMathActivity ::
  -- | 'name'
  Core.Text ->
  -- | 'attribute'
  Core.Text ->
  -- | 'math'
  Core.Text ->
  MathActivity
newMathActivity pName_ pAttribute_ pMath_ =
  MathActivity'
    { next = Core.Nothing,
      name = pName_,
      attribute = pAttribute_,
      math = pMath_
    }

-- | The next activity in the pipeline.
mathActivity_next :: Lens.Lens' MathActivity (Core.Maybe Core.Text)
mathActivity_next = Lens.lens (\MathActivity' {next} -> next) (\s@MathActivity' {} a -> s {next = a} :: MathActivity)

-- | The name of the math activity.
mathActivity_name :: Lens.Lens' MathActivity Core.Text
mathActivity_name = Lens.lens (\MathActivity' {name} -> name) (\s@MathActivity' {} a -> s {name = a} :: MathActivity)

-- | The name of the attribute that contains the result of the math
-- operation.
mathActivity_attribute :: Lens.Lens' MathActivity Core.Text
mathActivity_attribute = Lens.lens (\MathActivity' {attribute} -> attribute) (\s@MathActivity' {} a -> s {attribute = a} :: MathActivity)

-- | An expression that uses one or more existing attributes and must return
-- an integer value.
mathActivity_math :: Lens.Lens' MathActivity Core.Text
mathActivity_math = Lens.lens (\MathActivity' {math} -> math) (\s@MathActivity' {} a -> s {math = a} :: MathActivity)

instance Core.FromJSON MathActivity where
  parseJSON =
    Core.withObject
      "MathActivity"
      ( \x ->
          MathActivity'
            Core.<$> (x Core..:? "next")
            Core.<*> (x Core..: "name")
            Core.<*> (x Core..: "attribute")
            Core.<*> (x Core..: "math")
      )

instance Core.Hashable MathActivity

instance Core.NFData MathActivity

instance Core.ToJSON MathActivity where
  toJSON MathActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("next" Core..=) Core.<$> next,
            Core.Just ("name" Core..= name),
            Core.Just ("attribute" Core..= attribute),
            Core.Just ("math" Core..= math)
          ]
      )
