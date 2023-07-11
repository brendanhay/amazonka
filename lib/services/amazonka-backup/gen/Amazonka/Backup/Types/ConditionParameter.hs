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
-- Module      : Amazonka.Backup.Types.ConditionParameter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.ConditionParameter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Includes information about tags you define to assign tagged resources to
-- a backup plan.
--
-- /See:/ 'newConditionParameter' smart constructor.
data ConditionParameter = ConditionParameter'
  { -- | The key in a key-value pair. For example, in the tag
    -- @Department: Accounting@, @Department@ is the key.
    conditionKey :: Prelude.Maybe Prelude.Text,
    -- | The value in a key-value pair. For example, in the tag
    -- @Department: Accounting@, @Accounting@ is the value.
    conditionValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConditionParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionKey', 'conditionParameter_conditionKey' - The key in a key-value pair. For example, in the tag
-- @Department: Accounting@, @Department@ is the key.
--
-- 'conditionValue', 'conditionParameter_conditionValue' - The value in a key-value pair. For example, in the tag
-- @Department: Accounting@, @Accounting@ is the value.
newConditionParameter ::
  ConditionParameter
newConditionParameter =
  ConditionParameter'
    { conditionKey = Prelude.Nothing,
      conditionValue = Prelude.Nothing
    }

-- | The key in a key-value pair. For example, in the tag
-- @Department: Accounting@, @Department@ is the key.
conditionParameter_conditionKey :: Lens.Lens' ConditionParameter (Prelude.Maybe Prelude.Text)
conditionParameter_conditionKey = Lens.lens (\ConditionParameter' {conditionKey} -> conditionKey) (\s@ConditionParameter' {} a -> s {conditionKey = a} :: ConditionParameter)

-- | The value in a key-value pair. For example, in the tag
-- @Department: Accounting@, @Accounting@ is the value.
conditionParameter_conditionValue :: Lens.Lens' ConditionParameter (Prelude.Maybe Prelude.Text)
conditionParameter_conditionValue = Lens.lens (\ConditionParameter' {conditionValue} -> conditionValue) (\s@ConditionParameter' {} a -> s {conditionValue = a} :: ConditionParameter)

instance Data.FromJSON ConditionParameter where
  parseJSON =
    Data.withObject
      "ConditionParameter"
      ( \x ->
          ConditionParameter'
            Prelude.<$> (x Data..:? "ConditionKey")
            Prelude.<*> (x Data..:? "ConditionValue")
      )

instance Prelude.Hashable ConditionParameter where
  hashWithSalt _salt ConditionParameter' {..} =
    _salt
      `Prelude.hashWithSalt` conditionKey
      `Prelude.hashWithSalt` conditionValue

instance Prelude.NFData ConditionParameter where
  rnf ConditionParameter' {..} =
    Prelude.rnf conditionKey
      `Prelude.seq` Prelude.rnf conditionValue

instance Data.ToJSON ConditionParameter where
  toJSON ConditionParameter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionKey" Data..=) Prelude.<$> conditionKey,
            ("ConditionValue" Data..=)
              Prelude.<$> conditionValue
          ]
      )
