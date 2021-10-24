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
-- Module      : Network.AWS.DirectConnect.Types.CustomerAgreement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.CustomerAgreement where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The name and status of a customer agreement.
--
-- /See:/ 'newCustomerAgreement' smart constructor.
data CustomerAgreement = CustomerAgreement'
  { -- | The status of the customer agreement. This will be either @signed@ or
    -- @unsigned@
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the agreement.
    agreementName :: Prelude.Maybe Prelude.Text
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
-- 'status', 'customerAgreement_status' - The status of the customer agreement. This will be either @signed@ or
-- @unsigned@
--
-- 'agreementName', 'customerAgreement_agreementName' - The name of the agreement.
newCustomerAgreement ::
  CustomerAgreement
newCustomerAgreement =
  CustomerAgreement'
    { status = Prelude.Nothing,
      agreementName = Prelude.Nothing
    }

-- | The status of the customer agreement. This will be either @signed@ or
-- @unsigned@
customerAgreement_status :: Lens.Lens' CustomerAgreement (Prelude.Maybe Prelude.Text)
customerAgreement_status = Lens.lens (\CustomerAgreement' {status} -> status) (\s@CustomerAgreement' {} a -> s {status = a} :: CustomerAgreement)

-- | The name of the agreement.
customerAgreement_agreementName :: Lens.Lens' CustomerAgreement (Prelude.Maybe Prelude.Text)
customerAgreement_agreementName = Lens.lens (\CustomerAgreement' {agreementName} -> agreementName) (\s@CustomerAgreement' {} a -> s {agreementName = a} :: CustomerAgreement)

instance Core.FromJSON CustomerAgreement where
  parseJSON =
    Core.withObject
      "CustomerAgreement"
      ( \x ->
          CustomerAgreement'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "agreementName")
      )

instance Prelude.Hashable CustomerAgreement

instance Prelude.NFData CustomerAgreement
