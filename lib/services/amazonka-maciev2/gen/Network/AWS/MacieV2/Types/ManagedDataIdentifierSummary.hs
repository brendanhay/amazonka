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
-- Module      : Network.AWS.MacieV2.Types.ManagedDataIdentifierSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.ManagedDataIdentifierSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types.SensitiveDataItemCategory
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a managed data identifier. For additional
-- information, see
-- <https://docs.aws.amazon.com/macie/latest/user/managed-data-identifiers.html Using managed data identifiers>
-- in the /Amazon Macie User Guide/.
--
-- /See:/ 'newManagedDataIdentifierSummary' smart constructor.
data ManagedDataIdentifierSummary = ManagedDataIdentifierSummary'
  { -- | The category of sensitive data that the managed data identifier detects:
    -- CREDENTIALS, for credentials data such as private keys or Amazon Web
    -- Services secret keys; FINANCIAL_INFORMATION, for financial data such as
    -- credit card numbers; or, PERSONAL_INFORMATION, for personal health
    -- information, such as health insurance identification numbers, or
    -- personally identifiable information, such as passport numbers.
    category :: Prelude.Maybe SensitiveDataItemCategory,
    -- | The unique identifier for the managed data identifier. This is a string
    -- that describes the type of sensitive data that the managed data
    -- identifier detects. For example: OPENSSH_PRIVATE_KEY for OpenSSH private
    -- keys, CREDIT_CARD_NUMBER for credit card numbers, or USA_PASSPORT_NUMBER
    -- for US passport numbers.
    id :: Prelude.Maybe Prelude.Text
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
-- 'category', 'managedDataIdentifierSummary_category' - The category of sensitive data that the managed data identifier detects:
-- CREDENTIALS, for credentials data such as private keys or Amazon Web
-- Services secret keys; FINANCIAL_INFORMATION, for financial data such as
-- credit card numbers; or, PERSONAL_INFORMATION, for personal health
-- information, such as health insurance identification numbers, or
-- personally identifiable information, such as passport numbers.
--
-- 'id', 'managedDataIdentifierSummary_id' - The unique identifier for the managed data identifier. This is a string
-- that describes the type of sensitive data that the managed data
-- identifier detects. For example: OPENSSH_PRIVATE_KEY for OpenSSH private
-- keys, CREDIT_CARD_NUMBER for credit card numbers, or USA_PASSPORT_NUMBER
-- for US passport numbers.
newManagedDataIdentifierSummary ::
  ManagedDataIdentifierSummary
newManagedDataIdentifierSummary =
  ManagedDataIdentifierSummary'
    { category =
        Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The category of sensitive data that the managed data identifier detects:
-- CREDENTIALS, for credentials data such as private keys or Amazon Web
-- Services secret keys; FINANCIAL_INFORMATION, for financial data such as
-- credit card numbers; or, PERSONAL_INFORMATION, for personal health
-- information, such as health insurance identification numbers, or
-- personally identifiable information, such as passport numbers.
managedDataIdentifierSummary_category :: Lens.Lens' ManagedDataIdentifierSummary (Prelude.Maybe SensitiveDataItemCategory)
managedDataIdentifierSummary_category = Lens.lens (\ManagedDataIdentifierSummary' {category} -> category) (\s@ManagedDataIdentifierSummary' {} a -> s {category = a} :: ManagedDataIdentifierSummary)

-- | The unique identifier for the managed data identifier. This is a string
-- that describes the type of sensitive data that the managed data
-- identifier detects. For example: OPENSSH_PRIVATE_KEY for OpenSSH private
-- keys, CREDIT_CARD_NUMBER for credit card numbers, or USA_PASSPORT_NUMBER
-- for US passport numbers.
managedDataIdentifierSummary_id :: Lens.Lens' ManagedDataIdentifierSummary (Prelude.Maybe Prelude.Text)
managedDataIdentifierSummary_id = Lens.lens (\ManagedDataIdentifierSummary' {id} -> id) (\s@ManagedDataIdentifierSummary' {} a -> s {id = a} :: ManagedDataIdentifierSummary)

instance Core.FromJSON ManagedDataIdentifierSummary where
  parseJSON =
    Core.withObject
      "ManagedDataIdentifierSummary"
      ( \x ->
          ManagedDataIdentifierSummary'
            Prelude.<$> (x Core..:? "category")
            Prelude.<*> (x Core..:? "id")
      )

instance
  Prelude.Hashable
    ManagedDataIdentifierSummary

instance Prelude.NFData ManagedDataIdentifierSummary
