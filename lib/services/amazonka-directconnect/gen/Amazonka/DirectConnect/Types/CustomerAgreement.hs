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
-- Module      : Amazonka.DirectConnect.Types.CustomerAgreement
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.CustomerAgreement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The name and status of a customer agreement.
--
-- /See:/ 'newCustomerAgreement' smart constructor.
data CustomerAgreement = CustomerAgreement'
  { -- | The name of the agreement.
    agreementName :: Prelude.Maybe Prelude.Text,
    -- | The status of the customer agreement. This will be either @signed@ or
    -- @unsigned@
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerAgreement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agreementName', 'customerAgreement_agreementName' - The name of the agreement.
--
-- 'status', 'customerAgreement_status' - The status of the customer agreement. This will be either @signed@ or
-- @unsigned@
newCustomerAgreement ::
  CustomerAgreement
newCustomerAgreement =
  CustomerAgreement'
    { agreementName = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The name of the agreement.
customerAgreement_agreementName :: Lens.Lens' CustomerAgreement (Prelude.Maybe Prelude.Text)
customerAgreement_agreementName = Lens.lens (\CustomerAgreement' {agreementName} -> agreementName) (\s@CustomerAgreement' {} a -> s {agreementName = a} :: CustomerAgreement)

-- | The status of the customer agreement. This will be either @signed@ or
-- @unsigned@
customerAgreement_status :: Lens.Lens' CustomerAgreement (Prelude.Maybe Prelude.Text)
customerAgreement_status = Lens.lens (\CustomerAgreement' {status} -> status) (\s@CustomerAgreement' {} a -> s {status = a} :: CustomerAgreement)

instance Core.FromJSON CustomerAgreement where
  parseJSON =
    Core.withObject
      "CustomerAgreement"
      ( \x ->
          CustomerAgreement'
            Prelude.<$> (x Core..:? "agreementName")
            Prelude.<*> (x Core..:? "status")
      )

instance Prelude.Hashable CustomerAgreement where
  hashWithSalt _salt CustomerAgreement' {..} =
    _salt `Prelude.hashWithSalt` agreementName
      `Prelude.hashWithSalt` status

instance Prelude.NFData CustomerAgreement where
  rnf CustomerAgreement' {..} =
    Prelude.rnf agreementName
      `Prelude.seq` Prelude.rnf status
