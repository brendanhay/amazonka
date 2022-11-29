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
-- Module      : Amazonka.MacieV2.Types.ManagedDataIdentifierSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.ManagedDataIdentifierSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.SensitiveDataItemCategory
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a managed data identifier. For additional
-- information, see
-- <https://docs.aws.amazon.com/macie/latest/user/managed-data-identifiers.html Using managed data identifiers>
-- in the /Amazon Macie User Guide/.
--
-- /See:/ 'newManagedDataIdentifierSummary' smart constructor.
data ManagedDataIdentifierSummary = ManagedDataIdentifierSummary'
  { -- | The unique identifier for the managed data identifier. This is a string
    -- that describes the type of sensitive data that the managed data
    -- identifier detects. For example: OPENSSH_PRIVATE_KEY for OpenSSH private
    -- keys, CREDIT_CARD_NUMBER for credit card numbers, or USA_PASSPORT_NUMBER
    -- for US passport numbers.
    id :: Prelude.Maybe Prelude.Text,
    -- | The category of sensitive data that the managed data identifier detects:
    -- CREDENTIALS, for credentials data such as private keys or Amazon Web
    -- Services secret access keys; FINANCIAL_INFORMATION, for financial data
    -- such as credit card numbers; or, PERSONAL_INFORMATION, for personal
    -- health information, such as health insurance identification numbers, or
    -- personally identifiable information, such as passport numbers.
    category :: Prelude.Maybe SensitiveDataItemCategory
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedDataIdentifierSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'managedDataIdentifierSummary_id' - The unique identifier for the managed data identifier. This is a string
-- that describes the type of sensitive data that the managed data
-- identifier detects. For example: OPENSSH_PRIVATE_KEY for OpenSSH private
-- keys, CREDIT_CARD_NUMBER for credit card numbers, or USA_PASSPORT_NUMBER
-- for US passport numbers.
--
-- 'category', 'managedDataIdentifierSummary_category' - The category of sensitive data that the managed data identifier detects:
-- CREDENTIALS, for credentials data such as private keys or Amazon Web
-- Services secret access keys; FINANCIAL_INFORMATION, for financial data
-- such as credit card numbers; or, PERSONAL_INFORMATION, for personal
-- health information, such as health insurance identification numbers, or
-- personally identifiable information, such as passport numbers.
newManagedDataIdentifierSummary ::
  ManagedDataIdentifierSummary
newManagedDataIdentifierSummary =
  ManagedDataIdentifierSummary'
    { id = Prelude.Nothing,
      category = Prelude.Nothing
    }

-- | The unique identifier for the managed data identifier. This is a string
-- that describes the type of sensitive data that the managed data
-- identifier detects. For example: OPENSSH_PRIVATE_KEY for OpenSSH private
-- keys, CREDIT_CARD_NUMBER for credit card numbers, or USA_PASSPORT_NUMBER
-- for US passport numbers.
managedDataIdentifierSummary_id :: Lens.Lens' ManagedDataIdentifierSummary (Prelude.Maybe Prelude.Text)
managedDataIdentifierSummary_id = Lens.lens (\ManagedDataIdentifierSummary' {id} -> id) (\s@ManagedDataIdentifierSummary' {} a -> s {id = a} :: ManagedDataIdentifierSummary)

-- | The category of sensitive data that the managed data identifier detects:
-- CREDENTIALS, for credentials data such as private keys or Amazon Web
-- Services secret access keys; FINANCIAL_INFORMATION, for financial data
-- such as credit card numbers; or, PERSONAL_INFORMATION, for personal
-- health information, such as health insurance identification numbers, or
-- personally identifiable information, such as passport numbers.
managedDataIdentifierSummary_category :: Lens.Lens' ManagedDataIdentifierSummary (Prelude.Maybe SensitiveDataItemCategory)
managedDataIdentifierSummary_category = Lens.lens (\ManagedDataIdentifierSummary' {category} -> category) (\s@ManagedDataIdentifierSummary' {} a -> s {category = a} :: ManagedDataIdentifierSummary)

instance Core.FromJSON ManagedDataIdentifierSummary where
  parseJSON =
    Core.withObject
      "ManagedDataIdentifierSummary"
      ( \x ->
          ManagedDataIdentifierSummary'
            Prelude.<$> (x Core..:? "id")
            Prelude.<*> (x Core..:? "category")
      )

instance
  Prelude.Hashable
    ManagedDataIdentifierSummary
  where
  hashWithSalt _salt ManagedDataIdentifierSummary' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` category

instance Prelude.NFData ManagedDataIdentifierSummary where
  rnf ManagedDataIdentifierSummary' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf category
