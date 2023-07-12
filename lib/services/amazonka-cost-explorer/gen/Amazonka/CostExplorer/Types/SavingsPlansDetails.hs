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
-- Module      : Amazonka.CostExplorer.Types.SavingsPlansDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.SavingsPlansDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The attribute details on a specific Savings Plan.
--
-- /See:/ 'newSavingsPlansDetails' smart constructor.
data SavingsPlansDetails = SavingsPlansDetails'
  { -- | A group of instance types that Savings Plans applies to.
    instanceFamily :: Prelude.Maybe Prelude.Text,
    -- | The unique ID that\'s used to distinguish Savings Plans from one
    -- another.
    offeringId :: Prelude.Maybe Prelude.Text,
    -- | A collection of Amazon Web Services resources in a geographic area. Each
    -- Amazon Web Services Region is isolated and independent of the other
    -- Regions.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SavingsPlansDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamily', 'savingsPlansDetails_instanceFamily' - A group of instance types that Savings Plans applies to.
--
-- 'offeringId', 'savingsPlansDetails_offeringId' - The unique ID that\'s used to distinguish Savings Plans from one
-- another.
--
-- 'region', 'savingsPlansDetails_region' - A collection of Amazon Web Services resources in a geographic area. Each
-- Amazon Web Services Region is isolated and independent of the other
-- Regions.
newSavingsPlansDetails ::
  SavingsPlansDetails
newSavingsPlansDetails =
  SavingsPlansDetails'
    { instanceFamily =
        Prelude.Nothing,
      offeringId = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | A group of instance types that Savings Plans applies to.
savingsPlansDetails_instanceFamily :: Lens.Lens' SavingsPlansDetails (Prelude.Maybe Prelude.Text)
savingsPlansDetails_instanceFamily = Lens.lens (\SavingsPlansDetails' {instanceFamily} -> instanceFamily) (\s@SavingsPlansDetails' {} a -> s {instanceFamily = a} :: SavingsPlansDetails)

-- | The unique ID that\'s used to distinguish Savings Plans from one
-- another.
savingsPlansDetails_offeringId :: Lens.Lens' SavingsPlansDetails (Prelude.Maybe Prelude.Text)
savingsPlansDetails_offeringId = Lens.lens (\SavingsPlansDetails' {offeringId} -> offeringId) (\s@SavingsPlansDetails' {} a -> s {offeringId = a} :: SavingsPlansDetails)

-- | A collection of Amazon Web Services resources in a geographic area. Each
-- Amazon Web Services Region is isolated and independent of the other
-- Regions.
savingsPlansDetails_region :: Lens.Lens' SavingsPlansDetails (Prelude.Maybe Prelude.Text)
savingsPlansDetails_region = Lens.lens (\SavingsPlansDetails' {region} -> region) (\s@SavingsPlansDetails' {} a -> s {region = a} :: SavingsPlansDetails)

instance Data.FromJSON SavingsPlansDetails where
  parseJSON =
    Data.withObject
      "SavingsPlansDetails"
      ( \x ->
          SavingsPlansDetails'
            Prelude.<$> (x Data..:? "InstanceFamily")
            Prelude.<*> (x Data..:? "OfferingId")
            Prelude.<*> (x Data..:? "Region")
      )

instance Prelude.Hashable SavingsPlansDetails where
  hashWithSalt _salt SavingsPlansDetails' {..} =
    _salt
      `Prelude.hashWithSalt` instanceFamily
      `Prelude.hashWithSalt` offeringId
      `Prelude.hashWithSalt` region

instance Prelude.NFData SavingsPlansDetails where
  rnf SavingsPlansDetails' {..} =
    Prelude.rnf instanceFamily
      `Prelude.seq` Prelude.rnf offeringId
      `Prelude.seq` Prelude.rnf region
