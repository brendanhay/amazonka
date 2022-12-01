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
-- Module      : Amazonka.SNS.Types.BatchResultErrorEntry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SNS.Types.BatchResultErrorEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Gives a detailed description of failed messages in the batch.
--
-- /See:/ 'newBatchResultErrorEntry' smart constructor.
data BatchResultErrorEntry = BatchResultErrorEntry'
  { -- | A message explaining why the action failed on this entry.
    message :: Prelude.Maybe Prelude.Text,
    -- | The @Id@ of an entry in a batch request
    id :: Prelude.Text,
    -- | An error code representing why the action failed on this entry.
    code :: Prelude.Text,
    -- | Specifies whether the error happened due to the caller of the batch API
    -- action.
    senderFault :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchResultErrorEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'batchResultErrorEntry_message' - A message explaining why the action failed on this entry.
--
-- 'id', 'batchResultErrorEntry_id' - The @Id@ of an entry in a batch request
--
-- 'code', 'batchResultErrorEntry_code' - An error code representing why the action failed on this entry.
--
-- 'senderFault', 'batchResultErrorEntry_senderFault' - Specifies whether the error happened due to the caller of the batch API
-- action.
newBatchResultErrorEntry ::
  -- | 'id'
  Prelude.Text ->
  -- | 'code'
  Prelude.Text ->
  -- | 'senderFault'
  Prelude.Bool ->
  BatchResultErrorEntry
newBatchResultErrorEntry pId_ pCode_ pSenderFault_ =
  BatchResultErrorEntry'
    { message = Prelude.Nothing,
      id = pId_,
      code = pCode_,
      senderFault = pSenderFault_
    }

-- | A message explaining why the action failed on this entry.
batchResultErrorEntry_message :: Lens.Lens' BatchResultErrorEntry (Prelude.Maybe Prelude.Text)
batchResultErrorEntry_message = Lens.lens (\BatchResultErrorEntry' {message} -> message) (\s@BatchResultErrorEntry' {} a -> s {message = a} :: BatchResultErrorEntry)

-- | The @Id@ of an entry in a batch request
batchResultErrorEntry_id :: Lens.Lens' BatchResultErrorEntry Prelude.Text
batchResultErrorEntry_id = Lens.lens (\BatchResultErrorEntry' {id} -> id) (\s@BatchResultErrorEntry' {} a -> s {id = a} :: BatchResultErrorEntry)

-- | An error code representing why the action failed on this entry.
batchResultErrorEntry_code :: Lens.Lens' BatchResultErrorEntry Prelude.Text
batchResultErrorEntry_code = Lens.lens (\BatchResultErrorEntry' {code} -> code) (\s@BatchResultErrorEntry' {} a -> s {code = a} :: BatchResultErrorEntry)

-- | Specifies whether the error happened due to the caller of the batch API
-- action.
batchResultErrorEntry_senderFault :: Lens.Lens' BatchResultErrorEntry Prelude.Bool
batchResultErrorEntry_senderFault = Lens.lens (\BatchResultErrorEntry' {senderFault} -> senderFault) (\s@BatchResultErrorEntry' {} a -> s {senderFault = a} :: BatchResultErrorEntry)

instance Core.FromXML BatchResultErrorEntry where
  parseXML x =
    BatchResultErrorEntry'
      Prelude.<$> (x Core..@? "Message")
      Prelude.<*> (x Core..@ "Id")
      Prelude.<*> (x Core..@ "Code")
      Prelude.<*> (x Core..@ "SenderFault")

instance Prelude.Hashable BatchResultErrorEntry where
  hashWithSalt _salt BatchResultErrorEntry' {..} =
    _salt `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` senderFault

instance Prelude.NFData BatchResultErrorEntry where
  rnf BatchResultErrorEntry' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf senderFault
