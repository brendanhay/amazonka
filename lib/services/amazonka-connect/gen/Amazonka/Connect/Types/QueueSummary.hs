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
-- Module      : Amazonka.Connect.Types.QueueSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QueueSummary where

import Amazonka.Connect.Types.QueueType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains summary information about a queue.
--
-- /See:/ 'newQueueSummary' smart constructor.
data QueueSummary = QueueSummary'
  { -- | The name of the queue.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the queue.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the queue.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of queue.
    queueType :: Prelude.Maybe QueueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueueSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'queueSummary_name' - The name of the queue.
--
-- 'arn', 'queueSummary_arn' - The Amazon Resource Name (ARN) of the queue.
--
-- 'id', 'queueSummary_id' - The identifier of the queue.
--
-- 'queueType', 'queueSummary_queueType' - The type of queue.
newQueueSummary ::
  QueueSummary
newQueueSummary =
  QueueSummary'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      queueType = Prelude.Nothing
    }

-- | The name of the queue.
queueSummary_name :: Lens.Lens' QueueSummary (Prelude.Maybe Prelude.Text)
queueSummary_name = Lens.lens (\QueueSummary' {name} -> name) (\s@QueueSummary' {} a -> s {name = a} :: QueueSummary)

-- | The Amazon Resource Name (ARN) of the queue.
queueSummary_arn :: Lens.Lens' QueueSummary (Prelude.Maybe Prelude.Text)
queueSummary_arn = Lens.lens (\QueueSummary' {arn} -> arn) (\s@QueueSummary' {} a -> s {arn = a} :: QueueSummary)

-- | The identifier of the queue.
queueSummary_id :: Lens.Lens' QueueSummary (Prelude.Maybe Prelude.Text)
queueSummary_id = Lens.lens (\QueueSummary' {id} -> id) (\s@QueueSummary' {} a -> s {id = a} :: QueueSummary)

-- | The type of queue.
queueSummary_queueType :: Lens.Lens' QueueSummary (Prelude.Maybe QueueType)
queueSummary_queueType = Lens.lens (\QueueSummary' {queueType} -> queueType) (\s@QueueSummary' {} a -> s {queueType = a} :: QueueSummary)

instance Data.FromJSON QueueSummary where
  parseJSON =
    Data.withObject
      "QueueSummary"
      ( \x ->
          QueueSummary'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "QueueType")
      )

instance Prelude.Hashable QueueSummary where
  hashWithSalt _salt QueueSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` queueType

instance Prelude.NFData QueueSummary where
  rnf QueueSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf queueType
