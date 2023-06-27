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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.Condition where

import Amazonka.Backup.Types.ConditionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains an array of triplets made up of a condition type (such as
-- @StringEquals@), a key, and a value. Used to filter resources using
-- their tags and assign them to a backup plan. Case sensitive.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | An operation applied to a key-value pair used to assign resources to
    -- your backup plan. Condition only supports @StringEquals@. For more
    -- flexible assignment options, including @StringLike@ and the ability to
    -- exclude resources from your backup plan, use @Conditions@ (with an \"s\"
    -- on the end) for your
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BackupSelection.html BackupSelection>
    -- .
    conditionType :: ConditionType,
    -- | The key in a key-value pair. For example, in the tag
    -- @Department: Accounting@, @Department@ is the key.
    conditionKey :: Prelude.Text,
    -- | The value in a key-value pair. For example, in the tag
    -- @Department: Accounting@, @Accounting@ is the value.
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
-- 'conditionType', 'condition_conditionType' - An operation applied to a key-value pair used to assign resources to
-- your backup plan. Condition only supports @StringEquals@. For more
-- flexible assignment options, including @StringLike@ and the ability to
-- exclude resources from your backup plan, use @Conditions@ (with an \"s\"
-- on the end) for your
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BackupSelection.html BackupSelection>
-- .
--
-- 'conditionKey', 'condition_conditionKey' - The key in a key-value pair. For example, in the tag
-- @Department: Accounting@, @Department@ is the key.
--
-- 'conditionValue', 'condition_conditionValue' - The value in a key-value pair. For example, in the tag
-- @Department: Accounting@, @Accounting@ is the value.
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

-- | An operation applied to a key-value pair used to assign resources to
-- your backup plan. Condition only supports @StringEquals@. For more
-- flexible assignment options, including @StringLike@ and the ability to
-- exclude resources from your backup plan, use @Conditions@ (with an \"s\"
-- on the end) for your
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BackupSelection.html BackupSelection>
-- .
condition_conditionType :: Lens.Lens' Condition ConditionType
condition_conditionType = Lens.lens (\Condition' {conditionType} -> conditionType) (\s@Condition' {} a -> s {conditionType = a} :: Condition)

-- | The key in a key-value pair. For example, in the tag
-- @Department: Accounting@, @Department@ is the key.
condition_conditionKey :: Lens.Lens' Condition Prelude.Text
condition_conditionKey = Lens.lens (\Condition' {conditionKey} -> conditionKey) (\s@Condition' {} a -> s {conditionKey = a} :: Condition)

-- | The value in a key-value pair. For example, in the tag
-- @Department: Accounting@, @Accounting@ is the value.
condition_conditionValue :: Lens.Lens' Condition Prelude.Text
condition_conditionValue = Lens.lens (\Condition' {conditionValue} -> conditionValue) (\s@Condition' {} a -> s {conditionValue = a} :: Condition)

instance Data.FromJSON Condition where
  parseJSON =
    Data.withObject
      "Condition"
      ( \x ->
          Condition'
            Prelude.<$> (x Data..: "ConditionType")
            Prelude.<*> (x Data..: "ConditionKey")
            Prelude.<*> (x Data..: "ConditionValue")
      )

instance Prelude.Hashable Condition where
  hashWithSalt _salt Condition' {..} =
    _salt
      `Prelude.hashWithSalt` conditionType
      `Prelude.hashWithSalt` conditionKey
      `Prelude.hashWithSalt` conditionValue

instance Prelude.NFData Condition where
  rnf Condition' {..} =
    Prelude.rnf conditionType
      `Prelude.seq` Prelude.rnf conditionKey
      `Prelude.seq` Prelude.rnf conditionValue

instance Data.ToJSON Condition where
  toJSON Condition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ConditionType" Data..= conditionType),
            Prelude.Just ("ConditionKey" Data..= conditionKey),
            Prelude.Just
              ("ConditionValue" Data..= conditionValue)
          ]
      )
