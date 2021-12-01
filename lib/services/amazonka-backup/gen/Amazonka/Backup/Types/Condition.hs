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
-- Module      : Amazonka.Backup.Types.Condition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.Condition where

import Amazonka.Backup.Types.ConditionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains an array of triplets made up of a condition type (such as
-- @StringEquals@), a key, and a value. Conditions are used to filter
-- resources in a selection that is assigned to a backup plan.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | An operation, such as @StringEquals@, that is applied to a key-value
    -- pair used to filter resources in a selection.
    conditionType :: ConditionType,
    -- | The key in a key-value pair. For example, in
    -- @\"ec2:ResourceTag\/Department\": \"accounting\"@,
    -- @\"ec2:ResourceTag\/Department\"@ is the key.
    conditionKey :: Prelude.Text,
    -- | The value in a key-value pair. For example, in
    -- @\"ec2:ResourceTag\/Department\": \"accounting\"@, @\"accounting\"@ is
    -- the value.
    conditionValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Condition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionType', 'condition_conditionType' - An operation, such as @StringEquals@, that is applied to a key-value
-- pair used to filter resources in a selection.
--
-- 'conditionKey', 'condition_conditionKey' - The key in a key-value pair. For example, in
-- @\"ec2:ResourceTag\/Department\": \"accounting\"@,
-- @\"ec2:ResourceTag\/Department\"@ is the key.
--
-- 'conditionValue', 'condition_conditionValue' - The value in a key-value pair. For example, in
-- @\"ec2:ResourceTag\/Department\": \"accounting\"@, @\"accounting\"@ is
-- the value.
newCondition ::
  -- | 'conditionType'
  ConditionType ->
  -- | 'conditionKey'
  Prelude.Text ->
  -- | 'conditionValue'
  Prelude.Text ->
  Condition
newCondition
  pConditionType_
  pConditionKey_
  pConditionValue_ =
    Condition'
      { conditionType = pConditionType_,
        conditionKey = pConditionKey_,
        conditionValue = pConditionValue_
      }

-- | An operation, such as @StringEquals@, that is applied to a key-value
-- pair used to filter resources in a selection.
condition_conditionType :: Lens.Lens' Condition ConditionType
condition_conditionType = Lens.lens (\Condition' {conditionType} -> conditionType) (\s@Condition' {} a -> s {conditionType = a} :: Condition)

-- | The key in a key-value pair. For example, in
-- @\"ec2:ResourceTag\/Department\": \"accounting\"@,
-- @\"ec2:ResourceTag\/Department\"@ is the key.
condition_conditionKey :: Lens.Lens' Condition Prelude.Text
condition_conditionKey = Lens.lens (\Condition' {conditionKey} -> conditionKey) (\s@Condition' {} a -> s {conditionKey = a} :: Condition)

-- | The value in a key-value pair. For example, in
-- @\"ec2:ResourceTag\/Department\": \"accounting\"@, @\"accounting\"@ is
-- the value.
condition_conditionValue :: Lens.Lens' Condition Prelude.Text
condition_conditionValue = Lens.lens (\Condition' {conditionValue} -> conditionValue) (\s@Condition' {} a -> s {conditionValue = a} :: Condition)

instance Core.FromJSON Condition where
  parseJSON =
    Core.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Core..: "ConditionType")
            Prelude.<*> (x Core..: "ConditionKey")
            Prelude.<*> (x Core..: "ConditionValue")
      )

instance Prelude.Hashable Condition where
  hashWithSalt salt' Condition' {..} =
    salt' `Prelude.hashWithSalt` conditionValue
      `Prelude.hashWithSalt` conditionKey
      `Prelude.hashWithSalt` conditionType

instance Prelude.NFData Condition where
  rnf Condition' {..} =
    Prelude.rnf conditionType
      `Prelude.seq` Prelude.rnf conditionValue
      `Prelude.seq` Prelude.rnf conditionKey

instance Core.ToJSON Condition where
  toJSON Condition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConditionType" Core..= conditionType),
            Prelude.Just ("ConditionKey" Core..= conditionKey),
            Prelude.Just
              ("ConditionValue" Core..= conditionValue)
          ]
      )
