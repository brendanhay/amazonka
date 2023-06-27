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
-- Module      : Amazonka.SSMIncidents.Types.Condition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.Condition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.AttributeValueList

-- | A conditional statement with which to compare a value, after a
-- timestamp, before a timestamp, or equal to a string or integer. If
-- multiple conditions are specified, the conditionals become an @AND@ed
-- statement. If multiple values are specified for a conditional, the
-- values are @OR@d.
--
-- /See:/ 'newCondition' smart constructor.
data Condition = Condition'
  { -- | After the specified timestamp.
    after :: Prelude.Maybe Data.POSIX,
    -- | Before the specified timestamp
    before :: Prelude.Maybe Data.POSIX,
    -- | The value is equal to the provided string or integer.
    equals :: Prelude.Maybe AttributeValueList
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
-- 'after', 'condition_after' - After the specified timestamp.
--
-- 'before', 'condition_before' - Before the specified timestamp
--
-- 'equals', 'condition_equals' - The value is equal to the provided string or integer.
newCondition ::
  Condition
newCondition =
  Condition'
    { after = Prelude.Nothing,
      before = Prelude.Nothing,
      equals = Prelude.Nothing
    }

-- | After the specified timestamp.
condition_after :: Lens.Lens' Condition (Prelude.Maybe Prelude.UTCTime)
condition_after = Lens.lens (\Condition' {after} -> after) (\s@Condition' {} a -> s {after = a} :: Condition) Prelude.. Lens.mapping Data._Time

-- | Before the specified timestamp
condition_before :: Lens.Lens' Condition (Prelude.Maybe Prelude.UTCTime)
condition_before = Lens.lens (\Condition' {before} -> before) (\s@Condition' {} a -> s {before = a} :: Condition) Prelude.. Lens.mapping Data._Time

-- | The value is equal to the provided string or integer.
condition_equals :: Lens.Lens' Condition (Prelude.Maybe AttributeValueList)
condition_equals = Lens.lens (\Condition' {equals} -> equals) (\s@Condition' {} a -> s {equals = a} :: Condition)

instance Prelude.Hashable Condition where
  hashWithSalt _salt Condition' {..} =
    _salt
      `Prelude.hashWithSalt` after
      `Prelude.hashWithSalt` before
      `Prelude.hashWithSalt` equals

instance Prelude.NFData Condition where
  rnf Condition' {..} =
    Prelude.rnf after
      `Prelude.seq` Prelude.rnf before
      `Prelude.seq` Prelude.rnf equals

instance Data.ToJSON Condition where
  toJSON Condition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("after" Data..=) Prelude.<$> after,
            ("before" Data..=) Prelude.<$> before,
            ("equals" Data..=) Prelude.<$> equals
          ]
      )
