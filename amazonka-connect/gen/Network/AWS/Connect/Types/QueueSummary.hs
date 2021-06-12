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
-- Module      : Network.AWS.Connect.Types.QueueSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QueueSummary where

import Network.AWS.Connect.Types.QueueType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains summary information about a queue.
--
-- /See:/ 'newQueueSummary' smart constructor.
data QueueSummary = QueueSummary'
  { -- | The type of queue.
    queueType :: Core.Maybe QueueType,
    -- | The Amazon Resource Name (ARN) of the queue.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the queue.
    id :: Core.Maybe Core.Text,
    -- | The name of the queue.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'QueueSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queueType', 'queueSummary_queueType' - The type of queue.
--
-- 'arn', 'queueSummary_arn' - The Amazon Resource Name (ARN) of the queue.
--
-- 'id', 'queueSummary_id' - The identifier of the queue.
--
-- 'name', 'queueSummary_name' - The name of the queue.
newQueueSummary ::
  QueueSummary
newQueueSummary =
  QueueSummary'
    { queueType = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

-- | The type of queue.
queueSummary_queueType :: Lens.Lens' QueueSummary (Core.Maybe QueueType)
queueSummary_queueType = Lens.lens (\QueueSummary' {queueType} -> queueType) (\s@QueueSummary' {} a -> s {queueType = a} :: QueueSummary)

-- | The Amazon Resource Name (ARN) of the queue.
queueSummary_arn :: Lens.Lens' QueueSummary (Core.Maybe Core.Text)
queueSummary_arn = Lens.lens (\QueueSummary' {arn} -> arn) (\s@QueueSummary' {} a -> s {arn = a} :: QueueSummary)

-- | The identifier of the queue.
queueSummary_id :: Lens.Lens' QueueSummary (Core.Maybe Core.Text)
queueSummary_id = Lens.lens (\QueueSummary' {id} -> id) (\s@QueueSummary' {} a -> s {id = a} :: QueueSummary)

-- | The name of the queue.
queueSummary_name :: Lens.Lens' QueueSummary (Core.Maybe Core.Text)
queueSummary_name = Lens.lens (\QueueSummary' {name} -> name) (\s@QueueSummary' {} a -> s {name = a} :: QueueSummary)

instance Core.FromJSON QueueSummary where
  parseJSON =
    Core.withObject
      "QueueSummary"
      ( \x ->
          QueueSummary'
            Core.<$> (x Core..:? "QueueType")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "Name")
      )

instance Core.Hashable QueueSummary

instance Core.NFData QueueSummary
