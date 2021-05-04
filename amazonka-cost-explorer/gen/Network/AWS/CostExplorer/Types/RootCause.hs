{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CostExplorer.Types.RootCause
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.RootCause where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The combination of AWS service, linked account, Region, and usage type
-- where a cost anomaly is observed.
--
-- /See:/ 'newRootCause' smart constructor.
data RootCause = RootCause'
  { -- | The AWS service name associated with the cost anomaly.
    service :: Prelude.Maybe Prelude.Text,
    -- | The @UsageType@ value associated with the cost anomaly.
    usageType :: Prelude.Maybe Prelude.Text,
    -- | The linked account value associated with the cost anomaly.
    linkedAccount :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region associated with the cost anomaly.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RootCause' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'service', 'rootCause_service' - The AWS service name associated with the cost anomaly.
--
-- 'usageType', 'rootCause_usageType' - The @UsageType@ value associated with the cost anomaly.
--
-- 'linkedAccount', 'rootCause_linkedAccount' - The linked account value associated with the cost anomaly.
--
-- 'region', 'rootCause_region' - The AWS Region associated with the cost anomaly.
newRootCause ::
  RootCause
newRootCause =
  RootCause'
    { service = Prelude.Nothing,
      usageType = Prelude.Nothing,
      linkedAccount = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The AWS service name associated with the cost anomaly.
rootCause_service :: Lens.Lens' RootCause (Prelude.Maybe Prelude.Text)
rootCause_service = Lens.lens (\RootCause' {service} -> service) (\s@RootCause' {} a -> s {service = a} :: RootCause)

-- | The @UsageType@ value associated with the cost anomaly.
rootCause_usageType :: Lens.Lens' RootCause (Prelude.Maybe Prelude.Text)
rootCause_usageType = Lens.lens (\RootCause' {usageType} -> usageType) (\s@RootCause' {} a -> s {usageType = a} :: RootCause)

-- | The linked account value associated with the cost anomaly.
rootCause_linkedAccount :: Lens.Lens' RootCause (Prelude.Maybe Prelude.Text)
rootCause_linkedAccount = Lens.lens (\RootCause' {linkedAccount} -> linkedAccount) (\s@RootCause' {} a -> s {linkedAccount = a} :: RootCause)

-- | The AWS Region associated with the cost anomaly.
rootCause_region :: Lens.Lens' RootCause (Prelude.Maybe Prelude.Text)
rootCause_region = Lens.lens (\RootCause' {region} -> region) (\s@RootCause' {} a -> s {region = a} :: RootCause)

instance Prelude.FromJSON RootCause where
  parseJSON =
    Prelude.withObject
      "RootCause"
      ( \x ->
          RootCause'
            Prelude.<$> (x Prelude..:? "Service")
            Prelude.<*> (x Prelude..:? "UsageType")
            Prelude.<*> (x Prelude..:? "LinkedAccount")
            Prelude.<*> (x Prelude..:? "Region")
      )

instance Prelude.Hashable RootCause

instance Prelude.NFData RootCause
