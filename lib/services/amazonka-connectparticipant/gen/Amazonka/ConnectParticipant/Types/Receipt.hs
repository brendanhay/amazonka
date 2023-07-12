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
-- Module      : Amazonka.ConnectParticipant.Types.Receipt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Types.Receipt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The receipt for the message delivered to the recipient.
--
-- /See:/ 'newReceipt' smart constructor.
data Receipt = Receipt'
  { -- | The time when the message was delivered to the recipient.
    deliveredTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The time when the message was read by the recipient.
    readTimestamp :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the recipient of the message.
    recipientParticipantId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Receipt' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveredTimestamp', 'receipt_deliveredTimestamp' - The time when the message was delivered to the recipient.
--
-- 'readTimestamp', 'receipt_readTimestamp' - The time when the message was read by the recipient.
--
-- 'recipientParticipantId', 'receipt_recipientParticipantId' - The identifier of the recipient of the message.
newReceipt ::
  Receipt
newReceipt =
  Receipt'
    { deliveredTimestamp = Prelude.Nothing,
      readTimestamp = Prelude.Nothing,
      recipientParticipantId = Prelude.Nothing
    }

-- | The time when the message was delivered to the recipient.
receipt_deliveredTimestamp :: Lens.Lens' Receipt (Prelude.Maybe Prelude.Text)
receipt_deliveredTimestamp = Lens.lens (\Receipt' {deliveredTimestamp} -> deliveredTimestamp) (\s@Receipt' {} a -> s {deliveredTimestamp = a} :: Receipt)

-- | The time when the message was read by the recipient.
receipt_readTimestamp :: Lens.Lens' Receipt (Prelude.Maybe Prelude.Text)
receipt_readTimestamp = Lens.lens (\Receipt' {readTimestamp} -> readTimestamp) (\s@Receipt' {} a -> s {readTimestamp = a} :: Receipt)

-- | The identifier of the recipient of the message.
receipt_recipientParticipantId :: Lens.Lens' Receipt (Prelude.Maybe Prelude.Text)
receipt_recipientParticipantId = Lens.lens (\Receipt' {recipientParticipantId} -> recipientParticipantId) (\s@Receipt' {} a -> s {recipientParticipantId = a} :: Receipt)

instance Data.FromJSON Receipt where
  parseJSON =
    Data.withObject
      "Receipt"
      ( \x ->
          Receipt'
            Prelude.<$> (x Data..:? "DeliveredTimestamp")
            Prelude.<*> (x Data..:? "ReadTimestamp")
            Prelude.<*> (x Data..:? "RecipientParticipantId")
      )

instance Prelude.Hashable Receipt where
  hashWithSalt _salt Receipt' {..} =
    _salt
      `Prelude.hashWithSalt` deliveredTimestamp
      `Prelude.hashWithSalt` readTimestamp
      `Prelude.hashWithSalt` recipientParticipantId

instance Prelude.NFData Receipt where
  rnf Receipt' {..} =
    Prelude.rnf deliveredTimestamp
      `Prelude.seq` Prelude.rnf readTimestamp
      `Prelude.seq` Prelude.rnf recipientParticipantId
