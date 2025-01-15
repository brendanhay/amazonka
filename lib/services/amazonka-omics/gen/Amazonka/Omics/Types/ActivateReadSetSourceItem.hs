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
-- Module      : Amazonka.Omics.Types.ActivateReadSetSourceItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ActivateReadSetSourceItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReadSetActivationJobItemStatus
import qualified Amazonka.Prelude as Prelude

-- | A source for a read set activation job.
--
-- /See:/ 'newActivateReadSetSourceItem' smart constructor.
data ActivateReadSetSourceItem = ActivateReadSetSourceItem'
  { -- | The source\'s status message.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The source\'s read set ID.
    readSetId :: Prelude.Text,
    -- | The source\'s status.
    status :: ReadSetActivationJobItemStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivateReadSetSourceItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'activateReadSetSourceItem_statusMessage' - The source\'s status message.
--
-- 'readSetId', 'activateReadSetSourceItem_readSetId' - The source\'s read set ID.
--
-- 'status', 'activateReadSetSourceItem_status' - The source\'s status.
newActivateReadSetSourceItem ::
  -- | 'readSetId'
  Prelude.Text ->
  -- | 'status'
  ReadSetActivationJobItemStatus ->
  ActivateReadSetSourceItem
newActivateReadSetSourceItem pReadSetId_ pStatus_ =
  ActivateReadSetSourceItem'
    { statusMessage =
        Prelude.Nothing,
      readSetId = pReadSetId_,
      status = pStatus_
    }

-- | The source\'s status message.
activateReadSetSourceItem_statusMessage :: Lens.Lens' ActivateReadSetSourceItem (Prelude.Maybe Prelude.Text)
activateReadSetSourceItem_statusMessage = Lens.lens (\ActivateReadSetSourceItem' {statusMessage} -> statusMessage) (\s@ActivateReadSetSourceItem' {} a -> s {statusMessage = a} :: ActivateReadSetSourceItem)

-- | The source\'s read set ID.
activateReadSetSourceItem_readSetId :: Lens.Lens' ActivateReadSetSourceItem Prelude.Text
activateReadSetSourceItem_readSetId = Lens.lens (\ActivateReadSetSourceItem' {readSetId} -> readSetId) (\s@ActivateReadSetSourceItem' {} a -> s {readSetId = a} :: ActivateReadSetSourceItem)

-- | The source\'s status.
activateReadSetSourceItem_status :: Lens.Lens' ActivateReadSetSourceItem ReadSetActivationJobItemStatus
activateReadSetSourceItem_status = Lens.lens (\ActivateReadSetSourceItem' {status} -> status) (\s@ActivateReadSetSourceItem' {} a -> s {status = a} :: ActivateReadSetSourceItem)

instance Data.FromJSON ActivateReadSetSourceItem where
  parseJSON =
    Data.withObject
      "ActivateReadSetSourceItem"
      ( \x ->
          ActivateReadSetSourceItem'
            Prelude.<$> (x Data..:? "statusMessage")
            Prelude.<*> (x Data..: "readSetId")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable ActivateReadSetSourceItem where
  hashWithSalt _salt ActivateReadSetSourceItem' {..} =
    _salt
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` readSetId
      `Prelude.hashWithSalt` status

instance Prelude.NFData ActivateReadSetSourceItem where
  rnf ActivateReadSetSourceItem' {..} =
    Prelude.rnf statusMessage `Prelude.seq`
      Prelude.rnf readSetId `Prelude.seq`
        Prelude.rnf status
