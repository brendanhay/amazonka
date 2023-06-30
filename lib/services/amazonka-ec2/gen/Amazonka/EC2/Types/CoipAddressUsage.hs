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
-- Module      : Amazonka.EC2.Types.CoipAddressUsage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CoipAddressUsage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes address usage for a customer-owned address pool.
--
-- /See:/ 'newCoipAddressUsage' smart constructor.
data CoipAddressUsage = CoipAddressUsage'
  { -- | The allocation ID of the address.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services service.
    awsService :: Prelude.Maybe Prelude.Text,
    -- | The customer-owned IP address.
    coIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CoipAddressUsage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationId', 'coipAddressUsage_allocationId' - The allocation ID of the address.
--
-- 'awsAccountId', 'coipAddressUsage_awsAccountId' - The Amazon Web Services account ID.
--
-- 'awsService', 'coipAddressUsage_awsService' - The Amazon Web Services service.
--
-- 'coIp', 'coipAddressUsage_coIp' - The customer-owned IP address.
newCoipAddressUsage ::
  CoipAddressUsage
newCoipAddressUsage =
  CoipAddressUsage'
    { allocationId = Prelude.Nothing,
      awsAccountId = Prelude.Nothing,
      awsService = Prelude.Nothing,
      coIp = Prelude.Nothing
    }

-- | The allocation ID of the address.
coipAddressUsage_allocationId :: Lens.Lens' CoipAddressUsage (Prelude.Maybe Prelude.Text)
coipAddressUsage_allocationId = Lens.lens (\CoipAddressUsage' {allocationId} -> allocationId) (\s@CoipAddressUsage' {} a -> s {allocationId = a} :: CoipAddressUsage)

-- | The Amazon Web Services account ID.
coipAddressUsage_awsAccountId :: Lens.Lens' CoipAddressUsage (Prelude.Maybe Prelude.Text)
coipAddressUsage_awsAccountId = Lens.lens (\CoipAddressUsage' {awsAccountId} -> awsAccountId) (\s@CoipAddressUsage' {} a -> s {awsAccountId = a} :: CoipAddressUsage)

-- | The Amazon Web Services service.
coipAddressUsage_awsService :: Lens.Lens' CoipAddressUsage (Prelude.Maybe Prelude.Text)
coipAddressUsage_awsService = Lens.lens (\CoipAddressUsage' {awsService} -> awsService) (\s@CoipAddressUsage' {} a -> s {awsService = a} :: CoipAddressUsage)

-- | The customer-owned IP address.
coipAddressUsage_coIp :: Lens.Lens' CoipAddressUsage (Prelude.Maybe Prelude.Text)
coipAddressUsage_coIp = Lens.lens (\CoipAddressUsage' {coIp} -> coIp) (\s@CoipAddressUsage' {} a -> s {coIp = a} :: CoipAddressUsage)

instance Data.FromXML CoipAddressUsage where
  parseXML x =
    CoipAddressUsage'
      Prelude.<$> (x Data..@? "allocationId")
      Prelude.<*> (x Data..@? "awsAccountId")
      Prelude.<*> (x Data..@? "awsService")
      Prelude.<*> (x Data..@? "coIp")

instance Prelude.Hashable CoipAddressUsage where
  hashWithSalt _salt CoipAddressUsage' {..} =
    _salt
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` awsService
      `Prelude.hashWithSalt` coIp

instance Prelude.NFData CoipAddressUsage where
  rnf CoipAddressUsage' {..} =
    Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf awsService
      `Prelude.seq` Prelude.rnf coIp
