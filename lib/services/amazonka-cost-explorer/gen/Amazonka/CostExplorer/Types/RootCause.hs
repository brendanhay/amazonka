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
-- Module      : Amazonka.CostExplorer.Types.RootCause
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.RootCause where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The combination of Amazon Web Service, linked account, Region, and usage
-- type where a cost anomaly is observed.
--
-- /See:/ 'newRootCause' smart constructor.
data RootCause = RootCause'
  { -- | The @UsageType@ value that\'s associated with the cost anomaly.
    usageType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Service name that\'s associated with the cost anomaly.
    service :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region that\'s associated with the cost anomaly.
    region :: Prelude.Maybe Prelude.Text,
    -- | The member account value that\'s associated with the cost anomaly.
    linkedAccount :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RootCause' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageType', 'rootCause_usageType' - The @UsageType@ value that\'s associated with the cost anomaly.
--
-- 'service', 'rootCause_service' - The Amazon Web Service name that\'s associated with the cost anomaly.
--
-- 'region', 'rootCause_region' - The Amazon Web Services Region that\'s associated with the cost anomaly.
--
-- 'linkedAccount', 'rootCause_linkedAccount' - The member account value that\'s associated with the cost anomaly.
newRootCause ::
  RootCause
newRootCause =
  RootCause'
    { usageType = Prelude.Nothing,
      service = Prelude.Nothing,
      region = Prelude.Nothing,
      linkedAccount = Prelude.Nothing
    }

-- | The @UsageType@ value that\'s associated with the cost anomaly.
rootCause_usageType :: Lens.Lens' RootCause (Prelude.Maybe Prelude.Text)
rootCause_usageType = Lens.lens (\RootCause' {usageType} -> usageType) (\s@RootCause' {} a -> s {usageType = a} :: RootCause)

-- | The Amazon Web Service name that\'s associated with the cost anomaly.
rootCause_service :: Lens.Lens' RootCause (Prelude.Maybe Prelude.Text)
rootCause_service = Lens.lens (\RootCause' {service} -> service) (\s@RootCause' {} a -> s {service = a} :: RootCause)

-- | The Amazon Web Services Region that\'s associated with the cost anomaly.
rootCause_region :: Lens.Lens' RootCause (Prelude.Maybe Prelude.Text)
rootCause_region = Lens.lens (\RootCause' {region} -> region) (\s@RootCause' {} a -> s {region = a} :: RootCause)

-- | The member account value that\'s associated with the cost anomaly.
rootCause_linkedAccount :: Lens.Lens' RootCause (Prelude.Maybe Prelude.Text)
rootCause_linkedAccount = Lens.lens (\RootCause' {linkedAccount} -> linkedAccount) (\s@RootCause' {} a -> s {linkedAccount = a} :: RootCause)

instance Data.FromJSON RootCause where
  parseJSON =
    Data.withObject
      "RootCause"
      ( \x ->
          RootCause'
            Prelude.<$> (x Data..:? "UsageType")
            Prelude.<*> (x Data..:? "Service")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "LinkedAccount")
      )

instance Prelude.Hashable RootCause where
  hashWithSalt _salt RootCause' {..} =
    _salt `Prelude.hashWithSalt` usageType
      `Prelude.hashWithSalt` service
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` linkedAccount

instance Prelude.NFData RootCause where
  rnf RootCause' {..} =
    Prelude.rnf usageType
      `Prelude.seq` Prelude.rnf service
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf linkedAccount
