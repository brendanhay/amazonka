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
-- Module      : Amazonka.WAFV2.Types.RateBasedStatementManagedKeysIPSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.RateBasedStatementManagedKeysIPSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WAFV2.Types.IPAddressVersion

-- | The set of IP addresses that are currently blocked for a
-- RateBasedStatement.
--
-- /See:/ 'newRateBasedStatementManagedKeysIPSet' smart constructor.
data RateBasedStatementManagedKeysIPSet = RateBasedStatementManagedKeysIPSet'
  { -- | The IP addresses that are currently blocked.
    addresses :: Prelude.Maybe [Prelude.Text],
    -- | The version of the IP addresses, either @IPV4@ or @IPV6@.
    iPAddressVersion :: Prelude.Maybe IPAddressVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RateBasedStatementManagedKeysIPSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addresses', 'rateBasedStatementManagedKeysIPSet_addresses' - The IP addresses that are currently blocked.
--
-- 'iPAddressVersion', 'rateBasedStatementManagedKeysIPSet_iPAddressVersion' - The version of the IP addresses, either @IPV4@ or @IPV6@.
newRateBasedStatementManagedKeysIPSet ::
  RateBasedStatementManagedKeysIPSet
newRateBasedStatementManagedKeysIPSet =
  RateBasedStatementManagedKeysIPSet'
    { addresses =
        Prelude.Nothing,
      iPAddressVersion = Prelude.Nothing
    }

-- | The IP addresses that are currently blocked.
rateBasedStatementManagedKeysIPSet_addresses :: Lens.Lens' RateBasedStatementManagedKeysIPSet (Prelude.Maybe [Prelude.Text])
rateBasedStatementManagedKeysIPSet_addresses = Lens.lens (\RateBasedStatementManagedKeysIPSet' {addresses} -> addresses) (\s@RateBasedStatementManagedKeysIPSet' {} a -> s {addresses = a} :: RateBasedStatementManagedKeysIPSet) Prelude.. Lens.mapping Lens.coerced

-- | The version of the IP addresses, either @IPV4@ or @IPV6@.
rateBasedStatementManagedKeysIPSet_iPAddressVersion :: Lens.Lens' RateBasedStatementManagedKeysIPSet (Prelude.Maybe IPAddressVersion)
rateBasedStatementManagedKeysIPSet_iPAddressVersion = Lens.lens (\RateBasedStatementManagedKeysIPSet' {iPAddressVersion} -> iPAddressVersion) (\s@RateBasedStatementManagedKeysIPSet' {} a -> s {iPAddressVersion = a} :: RateBasedStatementManagedKeysIPSet)

instance
  Data.FromJSON
    RateBasedStatementManagedKeysIPSet
  where
  parseJSON =
    Data.withObject
      "RateBasedStatementManagedKeysIPSet"
      ( \x ->
          RateBasedStatementManagedKeysIPSet'
            Prelude.<$> (x Data..:? "Addresses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "IPAddressVersion")
      )

instance
  Prelude.Hashable
    RateBasedStatementManagedKeysIPSet
  where
  hashWithSalt
    _salt
    RateBasedStatementManagedKeysIPSet' {..} =
      _salt
        `Prelude.hashWithSalt` addresses
        `Prelude.hashWithSalt` iPAddressVersion

instance
  Prelude.NFData
    RateBasedStatementManagedKeysIPSet
  where
  rnf RateBasedStatementManagedKeysIPSet' {..} =
    Prelude.rnf addresses `Prelude.seq`
      Prelude.rnf iPAddressVersion
