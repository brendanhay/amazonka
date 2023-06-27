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
-- Module      : Amazonka.SSMContacts.Types.Receipt
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.Receipt where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.ReceiptType

-- | Records events during an engagement.
--
-- /See:/ 'newReceipt' smart constructor.
data Receipt = Receipt'
  { -- | The Amazon Resource Name (ARN) of the contact channel Incident Manager
    -- engaged.
    contactChannelArn :: Prelude.Maybe Prelude.Text,
    -- | Information provided during the page acknowledgement.
    receiptInfo :: Prelude.Maybe Prelude.Text,
    -- | The type follows the engagement cycle, @SENT@, @DELIVERED@, and @READ@.
    receiptType :: ReceiptType,
    -- | The time receipt was @SENT@, @DELIVERED@, or @READ@.
    receiptTime :: Data.POSIX
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
-- 'contactChannelArn', 'receipt_contactChannelArn' - The Amazon Resource Name (ARN) of the contact channel Incident Manager
-- engaged.
--
-- 'receiptInfo', 'receipt_receiptInfo' - Information provided during the page acknowledgement.
--
-- 'receiptType', 'receipt_receiptType' - The type follows the engagement cycle, @SENT@, @DELIVERED@, and @READ@.
--
-- 'receiptTime', 'receipt_receiptTime' - The time receipt was @SENT@, @DELIVERED@, or @READ@.
newReceipt ::
  -- | 'receiptType'
  ReceiptType ->
  -- | 'receiptTime'
  Prelude.UTCTime ->
  Receipt
newReceipt pReceiptType_ pReceiptTime_ =
  Receipt'
    { contactChannelArn = Prelude.Nothing,
      receiptInfo = Prelude.Nothing,
      receiptType = pReceiptType_,
      receiptTime = Data._Time Lens.# pReceiptTime_
    }

-- | The Amazon Resource Name (ARN) of the contact channel Incident Manager
-- engaged.
receipt_contactChannelArn :: Lens.Lens' Receipt (Prelude.Maybe Prelude.Text)
receipt_contactChannelArn = Lens.lens (\Receipt' {contactChannelArn} -> contactChannelArn) (\s@Receipt' {} a -> s {contactChannelArn = a} :: Receipt)

-- | Information provided during the page acknowledgement.
receipt_receiptInfo :: Lens.Lens' Receipt (Prelude.Maybe Prelude.Text)
receipt_receiptInfo = Lens.lens (\Receipt' {receiptInfo} -> receiptInfo) (\s@Receipt' {} a -> s {receiptInfo = a} :: Receipt)

-- | The type follows the engagement cycle, @SENT@, @DELIVERED@, and @READ@.
receipt_receiptType :: Lens.Lens' Receipt ReceiptType
receipt_receiptType = Lens.lens (\Receipt' {receiptType} -> receiptType) (\s@Receipt' {} a -> s {receiptType = a} :: Receipt)

-- | The time receipt was @SENT@, @DELIVERED@, or @READ@.
receipt_receiptTime :: Lens.Lens' Receipt Prelude.UTCTime
receipt_receiptTime = Lens.lens (\Receipt' {receiptTime} -> receiptTime) (\s@Receipt' {} a -> s {receiptTime = a} :: Receipt) Prelude.. Data._Time

instance Data.FromJSON Receipt where
  parseJSON =
    Data.withObject
      "Receipt"
      ( \x ->
          Receipt'
            Prelude.<$> (x Data..:? "ContactChannelArn")
            Prelude.<*> (x Data..:? "ReceiptInfo")
            Prelude.<*> (x Data..: "ReceiptType")
            Prelude.<*> (x Data..: "ReceiptTime")
      )

instance Prelude.Hashable Receipt where
  hashWithSalt _salt Receipt' {..} =
    _salt
      `Prelude.hashWithSalt` contactChannelArn
      `Prelude.hashWithSalt` receiptInfo
      `Prelude.hashWithSalt` receiptType
      `Prelude.hashWithSalt` receiptTime

instance Prelude.NFData Receipt where
  rnf Receipt' {..} =
    Prelude.rnf contactChannelArn
      `Prelude.seq` Prelude.rnf receiptInfo
      `Prelude.seq` Prelude.rnf receiptType
      `Prelude.seq` Prelude.rnf receiptTime
