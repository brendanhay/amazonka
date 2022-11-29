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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QueueSearchCriteria where

import Amazonka.Connect.Types.SearchableQueueType
import Amazonka.Connect.Types.StringCondition
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The search criteria to be used to return queues.
--
-- The @name@ and @description@ fields support \"contains\" queries with a
-- minimum of 2 characters and a maximum of 25 characters. Any queries with
-- character lengths outside of this range will throw invalid results.
--
-- /See:/ 'newQueueSearchCriteria' smart constructor.
data QueueSearchCriteria = QueueSearchCriteria'
  { stringCondition :: Prelude.Maybe StringCondition,
    -- | A list of conditions which would be applied together with an OR
    -- condition.
    orConditions :: Prelude.Maybe [QueueSearchCriteria],
    -- | A list of conditions which would be applied together with an AND
    -- condition.
    andConditions :: Prelude.Maybe [QueueSearchCriteria],
    -- | The type of queue.
    queueTypeCondition :: Prelude.Maybe SearchableQueueType
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
-- 'stringCondition', 'queueSearchCriteria_stringCondition' - Undocumented member.
--
-- 'orConditions', 'queueSearchCriteria_orConditions' - A list of conditions which would be applied together with an OR
-- condition.
--
-- 'andConditions', 'queueSearchCriteria_andConditions' - A list of conditions which would be applied together with an AND
-- condition.
--
-- 'queueTypeCondition', 'queueSearchCriteria_queueTypeCondition' - The type of queue.
newQueueSearchCriteria ::
  QueueSearchCriteria
newQueueSearchCriteria =
  QueueSearchCriteria'
    { stringCondition =
        Prelude.Nothing,
      orConditions = Prelude.Nothing,
      andConditions = Prelude.Nothing,
      queueTypeCondition = Prelude.Nothing
    }

-- | Undocumented member.
queueSearchCriteria_stringCondition :: Lens.Lens' QueueSearchCriteria (Prelude.Maybe StringCondition)
queueSearchCriteria_stringCondition = Lens.lens (\QueueSearchCriteria' {stringCondition} -> stringCondition) (\s@QueueSearchCriteria' {} a -> s {stringCondition = a} :: QueueSearchCriteria)

-- | A list of conditions which would be applied together with an OR
-- condition.
queueSearchCriteria_orConditions :: Lens.Lens' QueueSearchCriteria (Prelude.Maybe [QueueSearchCriteria])
queueSearchCriteria_orConditions = Lens.lens (\QueueSearchCriteria' {orConditions} -> orConditions) (\s@QueueSearchCriteria' {} a -> s {orConditions = a} :: QueueSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | A list of conditions which would be applied together with an AND
-- condition.
queueSearchCriteria_andConditions :: Lens.Lens' QueueSearchCriteria (Prelude.Maybe [QueueSearchCriteria])
queueSearchCriteria_andConditions = Lens.lens (\QueueSearchCriteria' {andConditions} -> andConditions) (\s@QueueSearchCriteria' {} a -> s {andConditions = a} :: QueueSearchCriteria) Prelude.. Lens.mapping Lens.coerced

-- | The type of queue.
queueSearchCriteria_queueTypeCondition :: Lens.Lens' QueueSearchCriteria (Prelude.Maybe SearchableQueueType)
queueSearchCriteria_queueTypeCondition = Lens.lens (\QueueSearchCriteria' {queueTypeCondition} -> queueTypeCondition) (\s@QueueSearchCriteria' {} a -> s {queueTypeCondition = a} :: QueueSearchCriteria)

instance Prelude.Hashable QueueSearchCriteria where
  hashWithSalt _salt QueueSearchCriteria' {..} =
    _salt `Prelude.hashWithSalt` stringCondition
      `Prelude.hashWithSalt` orConditions
      `Prelude.hashWithSalt` andConditions
      `Prelude.hashWithSalt` queueTypeCondition

instance Prelude.NFData QueueSearchCriteria where
  rnf QueueSearchCriteria' {..} =
    Prelude.rnf stringCondition
      `Prelude.seq` Prelude.rnf orConditions
      `Prelude.seq` Prelude.rnf andConditions
      `Prelude.seq` Prelude.rnf queueTypeCondition

instance Core.ToJSON QueueSearchCriteria where
  toJSON QueueSearchCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StringCondition" Core..=)
              Prelude.<$> stringCondition,
            ("OrConditions" Core..=) Prelude.<$> orConditions,
            ("AndConditions" Core..=) Prelude.<$> andConditions,
            ("QueueTypeCondition" Core..=)
              Prelude.<$> queueTypeCondition
          ]
      )
