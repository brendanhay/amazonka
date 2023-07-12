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
-- Module      : Amazonka.QuickSight.Types.JoinInstruction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.JoinInstruction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.JoinKeyProperties
import Amazonka.QuickSight.Types.JoinType

-- | The instructions associated with a join.
--
-- /See:/ 'newJoinInstruction' smart constructor.
data JoinInstruction = JoinInstruction'
  { -- | Join key properties of the left operand.
    leftJoinKeyProperties :: Prelude.Maybe JoinKeyProperties,
    -- | Join key properties of the right operand.
    rightJoinKeyProperties :: Prelude.Maybe JoinKeyProperties,
    -- | The operand on the left side of a join.
    leftOperand :: Prelude.Text,
    -- | The operand on the right side of a join.
    rightOperand :: Prelude.Text,
    -- | The type of join that it is.
    type' :: JoinType,
    -- | The join instructions provided in the @ON@ clause of a join.
    onClause :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JoinInstruction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'leftJoinKeyProperties', 'joinInstruction_leftJoinKeyProperties' - Join key properties of the left operand.
--
-- 'rightJoinKeyProperties', 'joinInstruction_rightJoinKeyProperties' - Join key properties of the right operand.
--
-- 'leftOperand', 'joinInstruction_leftOperand' - The operand on the left side of a join.
--
-- 'rightOperand', 'joinInstruction_rightOperand' - The operand on the right side of a join.
--
-- 'type'', 'joinInstruction_type' - The type of join that it is.
--
-- 'onClause', 'joinInstruction_onClause' - The join instructions provided in the @ON@ clause of a join.
newJoinInstruction ::
  -- | 'leftOperand'
  Prelude.Text ->
  -- | 'rightOperand'
  Prelude.Text ->
  -- | 'type''
  JoinType ->
  -- | 'onClause'
  Prelude.Text ->
  JoinInstruction
newJoinInstruction
  pLeftOperand_
  pRightOperand_
  pType_
  pOnClause_ =
    JoinInstruction'
      { leftJoinKeyProperties =
          Prelude.Nothing,
        rightJoinKeyProperties = Prelude.Nothing,
        leftOperand = pLeftOperand_,
        rightOperand = pRightOperand_,
        type' = pType_,
        onClause = pOnClause_
      }

-- | Join key properties of the left operand.
joinInstruction_leftJoinKeyProperties :: Lens.Lens' JoinInstruction (Prelude.Maybe JoinKeyProperties)
joinInstruction_leftJoinKeyProperties = Lens.lens (\JoinInstruction' {leftJoinKeyProperties} -> leftJoinKeyProperties) (\s@JoinInstruction' {} a -> s {leftJoinKeyProperties = a} :: JoinInstruction)

-- | Join key properties of the right operand.
joinInstruction_rightJoinKeyProperties :: Lens.Lens' JoinInstruction (Prelude.Maybe JoinKeyProperties)
joinInstruction_rightJoinKeyProperties = Lens.lens (\JoinInstruction' {rightJoinKeyProperties} -> rightJoinKeyProperties) (\s@JoinInstruction' {} a -> s {rightJoinKeyProperties = a} :: JoinInstruction)

-- | The operand on the left side of a join.
joinInstruction_leftOperand :: Lens.Lens' JoinInstruction Prelude.Text
joinInstruction_leftOperand = Lens.lens (\JoinInstruction' {leftOperand} -> leftOperand) (\s@JoinInstruction' {} a -> s {leftOperand = a} :: JoinInstruction)

-- | The operand on the right side of a join.
joinInstruction_rightOperand :: Lens.Lens' JoinInstruction Prelude.Text
joinInstruction_rightOperand = Lens.lens (\JoinInstruction' {rightOperand} -> rightOperand) (\s@JoinInstruction' {} a -> s {rightOperand = a} :: JoinInstruction)

-- | The type of join that it is.
joinInstruction_type :: Lens.Lens' JoinInstruction JoinType
joinInstruction_type = Lens.lens (\JoinInstruction' {type'} -> type') (\s@JoinInstruction' {} a -> s {type' = a} :: JoinInstruction)

-- | The join instructions provided in the @ON@ clause of a join.
joinInstruction_onClause :: Lens.Lens' JoinInstruction Prelude.Text
joinInstruction_onClause = Lens.lens (\JoinInstruction' {onClause} -> onClause) (\s@JoinInstruction' {} a -> s {onClause = a} :: JoinInstruction)

instance Data.FromJSON JoinInstruction where
  parseJSON =
    Data.withObject
      "JoinInstruction"
      ( \x ->
          JoinInstruction'
            Prelude.<$> (x Data..:? "LeftJoinKeyProperties")
            Prelude.<*> (x Data..:? "RightJoinKeyProperties")
            Prelude.<*> (x Data..: "LeftOperand")
            Prelude.<*> (x Data..: "RightOperand")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "OnClause")
      )

instance Prelude.Hashable JoinInstruction where
  hashWithSalt _salt JoinInstruction' {..} =
    _salt
      `Prelude.hashWithSalt` leftJoinKeyProperties
      `Prelude.hashWithSalt` rightJoinKeyProperties
      `Prelude.hashWithSalt` leftOperand
      `Prelude.hashWithSalt` rightOperand
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` onClause

instance Prelude.NFData JoinInstruction where
  rnf JoinInstruction' {..} =
    Prelude.rnf leftJoinKeyProperties
      `Prelude.seq` Prelude.rnf rightJoinKeyProperties
      `Prelude.seq` Prelude.rnf leftOperand
      `Prelude.seq` Prelude.rnf rightOperand
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf onClause

instance Data.ToJSON JoinInstruction where
  toJSON JoinInstruction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LeftJoinKeyProperties" Data..=)
              Prelude.<$> leftJoinKeyProperties,
            ("RightJoinKeyProperties" Data..=)
              Prelude.<$> rightJoinKeyProperties,
            Prelude.Just ("LeftOperand" Data..= leftOperand),
            Prelude.Just ("RightOperand" Data..= rightOperand),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just ("OnClause" Data..= onClause)
          ]
      )
