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
-- Module      : Amazonka.Connect.Types.QueueInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QueueInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | If this contact was queued, this contains information about the queue.
--
-- /See:/ 'newQueueInfo' smart constructor.
data QueueInfo = QueueInfo'
  { -- | The unique identifier for the queue.
    id :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the contact was added to the queue.
    enqueueTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueueInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'queueInfo_id' - The unique identifier for the queue.
--
-- 'enqueueTimestamp', 'queueInfo_enqueueTimestamp' - The timestamp when the contact was added to the queue.
newQueueInfo ::
  QueueInfo
newQueueInfo =
  QueueInfo'
    { id = Prelude.Nothing,
      enqueueTimestamp = Prelude.Nothing
    }

-- | The unique identifier for the queue.
queueInfo_id :: Lens.Lens' QueueInfo (Prelude.Maybe Prelude.Text)
queueInfo_id = Lens.lens (\QueueInfo' {id} -> id) (\s@QueueInfo' {} a -> s {id = a} :: QueueInfo)

-- | The timestamp when the contact was added to the queue.
queueInfo_enqueueTimestamp :: Lens.Lens' QueueInfo (Prelude.Maybe Prelude.UTCTime)
queueInfo_enqueueTimestamp = Lens.lens (\QueueInfo' {enqueueTimestamp} -> enqueueTimestamp) (\s@QueueInfo' {} a -> s {enqueueTimestamp = a} :: QueueInfo) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON QueueInfo where
  parseJSON =
    Core.withObject
      "QueueInfo"
      ( \x ->
          QueueInfo'
            Prelude.<$> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "EnqueueTimestamp")
      )

instance Prelude.Hashable QueueInfo where
  hashWithSalt _salt QueueInfo' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` enqueueTimestamp

instance Prelude.NFData QueueInfo where
  rnf QueueInfo' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf enqueueTimestamp
