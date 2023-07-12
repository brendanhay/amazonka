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
-- Module      : Amazonka.ConnectParticipant.Types.MessageMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectParticipant.Types.MessageMetadata where

import Amazonka.ConnectParticipant.Types.Receipt
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata related to a message.
--
-- /See:/ 'newMessageMetadata' smart constructor.
data MessageMetadata = MessageMetadata'
  { -- | The identifier of the message that contains the metadata information.
    messageId :: Prelude.Maybe Prelude.Text,
    -- | The list of receipt information for a message for different recipients.
    receipts :: Prelude.Maybe [Receipt]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'messageId', 'messageMetadata_messageId' - The identifier of the message that contains the metadata information.
--
-- 'receipts', 'messageMetadata_receipts' - The list of receipt information for a message for different recipients.
newMessageMetadata ::
  MessageMetadata
newMessageMetadata =
  MessageMetadata'
    { messageId = Prelude.Nothing,
      receipts = Prelude.Nothing
    }

-- | The identifier of the message that contains the metadata information.
messageMetadata_messageId :: Lens.Lens' MessageMetadata (Prelude.Maybe Prelude.Text)
messageMetadata_messageId = Lens.lens (\MessageMetadata' {messageId} -> messageId) (\s@MessageMetadata' {} a -> s {messageId = a} :: MessageMetadata)

-- | The list of receipt information for a message for different recipients.
messageMetadata_receipts :: Lens.Lens' MessageMetadata (Prelude.Maybe [Receipt])
messageMetadata_receipts = Lens.lens (\MessageMetadata' {receipts} -> receipts) (\s@MessageMetadata' {} a -> s {receipts = a} :: MessageMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MessageMetadata where
  parseJSON =
    Data.withObject
      "MessageMetadata"
      ( \x ->
          MessageMetadata'
            Prelude.<$> (x Data..:? "MessageId")
            Prelude.<*> (x Data..:? "Receipts" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable MessageMetadata where
  hashWithSalt _salt MessageMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` messageId
      `Prelude.hashWithSalt` receipts

instance Prelude.NFData MessageMetadata where
  rnf MessageMetadata' {..} =
    Prelude.rnf messageId
      `Prelude.seq` Prelude.rnf receipts
