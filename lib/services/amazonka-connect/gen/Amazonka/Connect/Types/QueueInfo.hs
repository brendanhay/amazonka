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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QueueInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | If this contact was queued, this contains information about the queue.
--
-- /See:/ 'newQueueInfo' smart constructor.
data QueueInfo = QueueInfo'
  { -- | The timestamp when the contact was added to the queue.
    enqueueTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The unique identifier for the queue.
    id :: Prelude.Maybe Prelude.Text
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
-- 'enqueueTimestamp', 'queueInfo_enqueueTimestamp' - The timestamp when the contact was added to the queue.
--
-- 'id', 'queueInfo_id' - The unique identifier for the queue.
newQueueInfo ::
  QueueInfo
newQueueInfo =
  QueueInfo'
    { enqueueTimestamp = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The timestamp when the contact was added to the queue.
queueInfo_enqueueTimestamp :: Lens.Lens' QueueInfo (Prelude.Maybe Prelude.UTCTime)
queueInfo_enqueueTimestamp = Lens.lens (\QueueInfo' {enqueueTimestamp} -> enqueueTimestamp) (\s@QueueInfo' {} a -> s {enqueueTimestamp = a} :: QueueInfo) Prelude.. Lens.mapping Data._Time

-- | The unique identifier for the queue.
queueInfo_id :: Lens.Lens' QueueInfo (Prelude.Maybe Prelude.Text)
queueInfo_id = Lens.lens (\QueueInfo' {id} -> id) (\s@QueueInfo' {} a -> s {id = a} :: QueueInfo)

instance Data.FromJSON QueueInfo where
  parseJSON =
    Data.withObject
      "QueueInfo"
      ( \x ->
          QueueInfo'
            Prelude.<$> (x Data..:? "EnqueueTimestamp")
            Prelude.<*> (x Data..:? "Id")
      )

instance Prelude.Hashable QueueInfo where
  hashWithSalt _salt QueueInfo' {..} =
    _salt
      `Prelude.hashWithSalt` enqueueTimestamp
      `Prelude.hashWithSalt` id

instance Prelude.NFData QueueInfo where
  rnf QueueInfo' {..} =
    Prelude.rnf enqueueTimestamp
      `Prelude.seq` Prelude.rnf id
