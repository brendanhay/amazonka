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
-- Module      : Network.AWS.IoTAnalytics.Types.MathActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.MathActivity where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An activity that computes an arithmetic expression using the message\'s
-- attributes.
--
-- /See:/ 'newMathActivity' smart constructor.
data MathActivity = MathActivity'
  { -- | The next activity in the pipeline.
    next :: Prelude.Maybe Prelude.Text,
    -- | The name of the math activity.
    name :: Prelude.Text,
    -- | The name of the attribute that contains the result of the math
    -- operation.
    attribute :: Prelude.Text,
    -- | An expression that uses one or more existing attributes and must return
    -- an integer value.
    math :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'attribute'
  Prelude.Text ->
  -- | 'math'
  Prelude.Text ->
  MathActivity
newMathActivity pName_ pAttribute_ pMath_ =
  MathActivity'
    { next = Prelude.Nothing,
      name = pName_,
      attribute = pAttribute_,
      math = pMath_
    }

-- | The next activity in the pipeline.
mathActivity_next :: Lens.Lens' MathActivity (Prelude.Maybe Prelude.Text)
mathActivity_next = Lens.lens (\MathActivity' {next} -> next) (\s@MathActivity' {} a -> s {next = a} :: MathActivity)

-- | The name of the math activity.
mathActivity_name :: Lens.Lens' MathActivity Prelude.Text
mathActivity_name = Lens.lens (\MathActivity' {name} -> name) (\s@MathActivity' {} a -> s {name = a} :: MathActivity)

-- | The name of the attribute that contains the result of the math
-- operation.
mathActivity_attribute :: Lens.Lens' MathActivity Prelude.Text
mathActivity_attribute = Lens.lens (\MathActivity' {attribute} -> attribute) (\s@MathActivity' {} a -> s {attribute = a} :: MathActivity)

-- | An expression that uses one or more existing attributes and must return
-- an integer value.
mathActivity_math :: Lens.Lens' MathActivity Prelude.Text
mathActivity_math = Lens.lens (\MathActivity' {math} -> math) (\s@MathActivity' {} a -> s {math = a} :: MathActivity)

instance Prelude.FromJSON MathActivity where
  parseJSON =
    Prelude.withObject
      "MathActivity"
      ( \x ->
          MathActivity'
            Prelude.<$> (x Prelude..:? "next")
            Prelude.<*> (x Prelude..: "name")
            Prelude.<*> (x Prelude..: "attribute")
            Prelude.<*> (x Prelude..: "math")
      )

instance Prelude.Hashable MathActivity

instance Prelude.NFData MathActivity

instance Prelude.ToJSON MathActivity where
  toJSON MathActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("next" Prelude..=) Prelude.<$> next,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("attribute" Prelude..= attribute),
            Prelude.Just ("math" Prelude..= math)
          ]
      )
