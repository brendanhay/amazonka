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
-- Module      : Amazonka.PaymentCryptographyData.Types.SessionKeyDerivationValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PaymentCryptographyData.Types.SessionKeyDerivationValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Parameters to derive session key value using a MAC EMV algorithm.
--
-- /See:/ 'newSessionKeyDerivationValue' smart constructor.
data SessionKeyDerivationValue = SessionKeyDerivationValue'
  { -- | The cryptogram provided by the terminal during transaction processing.
    applicationCryptogram :: Prelude.Maybe Prelude.Text,
    -- | The transaction counter that is provided by the terminal during
    -- transaction processing.
    applicationTransactionCounter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionKeyDerivationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationCryptogram', 'sessionKeyDerivationValue_applicationCryptogram' - The cryptogram provided by the terminal during transaction processing.
--
-- 'applicationTransactionCounter', 'sessionKeyDerivationValue_applicationTransactionCounter' - The transaction counter that is provided by the terminal during
-- transaction processing.
newSessionKeyDerivationValue ::
  SessionKeyDerivationValue
newSessionKeyDerivationValue =
  SessionKeyDerivationValue'
    { applicationCryptogram =
        Prelude.Nothing,
      applicationTransactionCounter = Prelude.Nothing
    }

-- | The cryptogram provided by the terminal during transaction processing.
sessionKeyDerivationValue_applicationCryptogram :: Lens.Lens' SessionKeyDerivationValue (Prelude.Maybe Prelude.Text)
sessionKeyDerivationValue_applicationCryptogram = Lens.lens (\SessionKeyDerivationValue' {applicationCryptogram} -> applicationCryptogram) (\s@SessionKeyDerivationValue' {} a -> s {applicationCryptogram = a} :: SessionKeyDerivationValue)

-- | The transaction counter that is provided by the terminal during
-- transaction processing.
sessionKeyDerivationValue_applicationTransactionCounter :: Lens.Lens' SessionKeyDerivationValue (Prelude.Maybe Prelude.Text)
sessionKeyDerivationValue_applicationTransactionCounter = Lens.lens (\SessionKeyDerivationValue' {applicationTransactionCounter} -> applicationTransactionCounter) (\s@SessionKeyDerivationValue' {} a -> s {applicationTransactionCounter = a} :: SessionKeyDerivationValue)

instance Prelude.Hashable SessionKeyDerivationValue where
  hashWithSalt _salt SessionKeyDerivationValue' {..} =
    _salt
      `Prelude.hashWithSalt` applicationCryptogram
      `Prelude.hashWithSalt` applicationTransactionCounter

instance Prelude.NFData SessionKeyDerivationValue where
  rnf SessionKeyDerivationValue' {..} =
    Prelude.rnf applicationCryptogram
      `Prelude.seq` Prelude.rnf applicationTransactionCounter

instance Data.ToJSON SessionKeyDerivationValue where
  toJSON SessionKeyDerivationValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplicationCryptogram" Data..=)
              Prelude.<$> applicationCryptogram,
            ("ApplicationTransactionCounter" Data..=)
              Prelude.<$> applicationTransactionCounter
          ]
      )
