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
-- Module      : Amazonka.SES.Types.ReceiptRuleSetMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.ReceiptRuleSetMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a receipt rule set.
--
-- A receipt rule set is a collection of rules that specify what Amazon SES
-- should do with mail it receives on behalf of your account\'s verified
-- domains.
--
-- For information about setting up receipt rule sets, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-receipt-rule-set.html Amazon SES Developer Guide>.
--
-- /See:/ 'newReceiptRuleSetMetadata' smart constructor.
data ReceiptRuleSetMetadata = ReceiptRuleSetMetadata'
  { -- | The name of the receipt rule set. The name must:
    --
    -- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
    --     underscores (_), or dashes (-).
    --
    -- -   Start and end with a letter or number.
    --
    -- -   Contain less than 64 characters.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date and time the receipt rule set was created.
    createdTimestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReceiptRuleSetMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'receiptRuleSetMetadata_name' - The name of the receipt rule set. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Start and end with a letter or number.
--
-- -   Contain less than 64 characters.
--
-- 'createdTimestamp', 'receiptRuleSetMetadata_createdTimestamp' - The date and time the receipt rule set was created.
newReceiptRuleSetMetadata ::
  ReceiptRuleSetMetadata
newReceiptRuleSetMetadata =
  ReceiptRuleSetMetadata'
    { name = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing
    }

-- | The name of the receipt rule set. The name must:
--
-- -   This value can only contain ASCII letters (a-z, A-Z), numbers (0-9),
--     underscores (_), or dashes (-).
--
-- -   Start and end with a letter or number.
--
-- -   Contain less than 64 characters.
receiptRuleSetMetadata_name :: Lens.Lens' ReceiptRuleSetMetadata (Prelude.Maybe Prelude.Text)
receiptRuleSetMetadata_name = Lens.lens (\ReceiptRuleSetMetadata' {name} -> name) (\s@ReceiptRuleSetMetadata' {} a -> s {name = a} :: ReceiptRuleSetMetadata)

-- | The date and time the receipt rule set was created.
receiptRuleSetMetadata_createdTimestamp :: Lens.Lens' ReceiptRuleSetMetadata (Prelude.Maybe Prelude.UTCTime)
receiptRuleSetMetadata_createdTimestamp = Lens.lens (\ReceiptRuleSetMetadata' {createdTimestamp} -> createdTimestamp) (\s@ReceiptRuleSetMetadata' {} a -> s {createdTimestamp = a} :: ReceiptRuleSetMetadata) Prelude.. Lens.mapping Data._Time

instance Data.FromXML ReceiptRuleSetMetadata where
  parseXML x =
    ReceiptRuleSetMetadata'
      Prelude.<$> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "CreatedTimestamp")

instance Prelude.Hashable ReceiptRuleSetMetadata where
  hashWithSalt _salt ReceiptRuleSetMetadata' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTimestamp

instance Prelude.NFData ReceiptRuleSetMetadata where
  rnf ReceiptRuleSetMetadata' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTimestamp
