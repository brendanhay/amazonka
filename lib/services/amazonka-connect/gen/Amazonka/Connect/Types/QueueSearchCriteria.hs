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
-- Module      : Amazonka.Connect.Types.QueueSearchCriteria
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QueueSearchCriteria where

import Amazonka.Connect.Types.SearchableQueueType
import Amazonka.Connect.Types.StringCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The search criteria to be used to return queues.
--
-- The @name@ and @description@ fields support \"contains\" queries with a
-- minimum of 2 characters and a maximum of 25 characters. Any queries with
-- character lengths outside of this range will throw invalid results.
--
-- /See:/ 'newQueueSearchCriteria' smart constructor.
data QueueSearchCriteria = QueueSearchCriteria'
  { -- | A list of conditions which would be applied together with an AND
    -- condition.
    andConditions :: Prelude.Maybe [QueueSearchCriteria],
    -- | A list of conditions which would be applied together with an OR
    -- condition.
    orConditions :: Prelude.Maybe [QueueSearchCriteria],
    -- | The type of queue.
    queueTypeCondition :: Prelude.Maybe SearchableQueueType,
    stringCondition :: Prelude.Maybe StringCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueueSearchCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'andConditions', 'queueSearchCriteria_andConditions' - A list of conditions which would be applied together with an AND
-- condition.
--
-- 'orConditions', 'queueSearchCriteria_orConditions' - A list of conditions which would be applied together with an OR
-- condition.
--
-- 'queueTypeCondition', 'queueSearchCriteria_queueTypeCondition' - The type of queue.
--
-- 'stringCondition', 'queueSearchCriteria_stringCondition' - Undocumented member.
newQueueSearchCriteria ::
  QueueSearchCriteria
newQueueSearchCriteria =
  QueueSearchCriteria'
    { andConditions =
        Prelude.Nothing,
      orConditions = Prelude.Nothing,
      queueTypeCondition = Prelude.Nothing,
      stringCondition = Prelude.Nothing
    }

-- | A list of conditions which would be applied together with an AND
-- condition.
queueSearchCriteria_andConditions :: Lens.Lens' QueueSearchCriteria (Prelude.Maybe [QueueSearchCriteria])
queueSearchCriteria_andConditions = Lens.lens (\QueueSearchCriteria' {andConditions} -> andConditions) (\s@QueueSearchCriteria' {} a -> s {andConditions = a} :: QueueSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A list of conditions which would be applied together with an OR
-- condition.
queueSearchCriteria_orConditions :: Lens.Lens' QueueSearchCriteria (Prelude.Maybe [QueueSearchCriteria])
queueSearchCriteria_orConditions = Lens.lens (\QueueSearchCriteria' {orConditions} -> orConditions) (\s@QueueSearchCriteria' {} a -> s {orConditions = a} :: QueueSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The type of queue.
queueSearchCriteria_queueTypeCondition :: Lens.Lens' QueueSearchCriteria (Prelude.Maybe SearchableQueueType)
queueSearchCriteria_queueTypeCondition = Lens.lens (\QueueSearchCriteria' {queueTypeCondition} -> queueTypeCondition) (\s@QueueSearchCriteria' {} a -> s {queueTypeCondition = a} :: QueueSearchCriteria)

-- | Undocumented member.
queueSearchCriteria_stringCondition :: Lens.Lens' QueueSearchCriteria (Prelude.Maybe StringCondition)
queueSearchCriteria_stringCondition = Lens.lens (\QueueSearchCriteria' {stringCondition} -> stringCondition) (\s@QueueSearchCriteria' {} a -> s {stringCondition = a} :: QueueSearchCriteria)

instance Prelude.Hashable QueueSearchCriteria where
  hashWithSalt _salt QueueSearchCriteria' {..} =
    _salt
      `Prelude.hashWithSalt` andConditions
      `Prelude.hashWithSalt` orConditions
      `Prelude.hashWithSalt` queueTypeCondition
      `Prelude.hashWithSalt` stringCondition

instance Prelude.NFData QueueSearchCriteria where
  rnf QueueSearchCriteria' {..} =
    Prelude.rnf andConditions `Prelude.seq`
      Prelude.rnf orConditions `Prelude.seq`
        Prelude.rnf queueTypeCondition `Prelude.seq`
          Prelude.rnf stringCondition

instance Data.ToJSON QueueSearchCriteria where
  toJSON QueueSearchCriteria' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AndConditions" Data..=) Prelude.<$> andConditions,
            ("OrConditions" Data..=) Prelude.<$> orConditions,
            ("QueueTypeCondition" Data..=)
              Prelude.<$> queueTypeCondition,
            ("StringCondition" Data..=)
              Prelude.<$> stringCondition
          ]
      )
