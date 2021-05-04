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
-- Module      : Network.AWS.Connect.Types.QueueSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.QueueSummary where

import Network.AWS.Connect.Types.QueueType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains summary information about a queue.
--
-- /See:/ 'newQueueSummary' smart constructor.
data QueueSummary = QueueSummary'
  { -- | The type of queue.
    queueType :: Prelude.Maybe QueueType,
    -- | The Amazon Resource Name (ARN) of the queue.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the queue.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the queue.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { queueType = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The type of queue.
queueSummary_queueType :: Lens.Lens' QueueSummary (Prelude.Maybe QueueType)
queueSummary_queueType = Lens.lens (\QueueSummary' {queueType} -> queueType) (\s@QueueSummary' {} a -> s {queueType = a} :: QueueSummary)

-- | The Amazon Resource Name (ARN) of the queue.
queueSummary_arn :: Lens.Lens' QueueSummary (Prelude.Maybe Prelude.Text)
queueSummary_arn = Lens.lens (\QueueSummary' {arn} -> arn) (\s@QueueSummary' {} a -> s {arn = a} :: QueueSummary)

-- | The identifier of the queue.
queueSummary_id :: Lens.Lens' QueueSummary (Prelude.Maybe Prelude.Text)
queueSummary_id = Lens.lens (\QueueSummary' {id} -> id) (\s@QueueSummary' {} a -> s {id = a} :: QueueSummary)

-- | The name of the queue.
queueSummary_name :: Lens.Lens' QueueSummary (Prelude.Maybe Prelude.Text)
queueSummary_name = Lens.lens (\QueueSummary' {name} -> name) (\s@QueueSummary' {} a -> s {name = a} :: QueueSummary)

instance Prelude.FromJSON QueueSummary where
  parseJSON =
    Prelude.withObject
      "QueueSummary"
      ( \x ->
          QueueSummary'
            Prelude.<$> (x Prelude..:? "QueueType")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable QueueSummary

instance Prelude.NFData QueueSummary
