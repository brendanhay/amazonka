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
-- Module      : Amazonka.IoTAnalytics.Types.DatasetContentDeliveryRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.DatasetContentDeliveryRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.DatasetContentDeliveryDestination
import qualified Amazonka.Prelude as Prelude

-- | When dataset contents are created, they are delivered to destination
-- specified here.
--
-- /See:/ 'newDatasetContentDeliveryRule' smart constructor.
data DatasetContentDeliveryRule = DatasetContentDeliveryRule'
  { -- | The name of the dataset content delivery rules entry.
    entryName :: Prelude.Maybe Prelude.Text,
    -- | The destination to which dataset contents are delivered.
    destination :: DatasetContentDeliveryDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetContentDeliveryRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entryName', 'datasetContentDeliveryRule_entryName' - The name of the dataset content delivery rules entry.
--
-- 'destination', 'datasetContentDeliveryRule_destination' - The destination to which dataset contents are delivered.
newDatasetContentDeliveryRule ::
  -- | 'destination'
  DatasetContentDeliveryDestination ->
  DatasetContentDeliveryRule
newDatasetContentDeliveryRule pDestination_ =
  DatasetContentDeliveryRule'
    { entryName =
        Prelude.Nothing,
      destination = pDestination_
    }

-- | The name of the dataset content delivery rules entry.
datasetContentDeliveryRule_entryName :: Lens.Lens' DatasetContentDeliveryRule (Prelude.Maybe Prelude.Text)
datasetContentDeliveryRule_entryName = Lens.lens (\DatasetContentDeliveryRule' {entryName} -> entryName) (\s@DatasetContentDeliveryRule' {} a -> s {entryName = a} :: DatasetContentDeliveryRule)

-- | The destination to which dataset contents are delivered.
datasetContentDeliveryRule_destination :: Lens.Lens' DatasetContentDeliveryRule DatasetContentDeliveryDestination
datasetContentDeliveryRule_destination = Lens.lens (\DatasetContentDeliveryRule' {destination} -> destination) (\s@DatasetContentDeliveryRule' {} a -> s {destination = a} :: DatasetContentDeliveryRule)

instance Data.FromJSON DatasetContentDeliveryRule where
  parseJSON =
    Data.withObject
      "DatasetContentDeliveryRule"
      ( \x ->
          DatasetContentDeliveryRule'
            Prelude.<$> (x Data..:? "entryName")
            Prelude.<*> (x Data..: "destination")
      )

instance Prelude.Hashable DatasetContentDeliveryRule where
  hashWithSalt _salt DatasetContentDeliveryRule' {..} =
    _salt `Prelude.hashWithSalt` entryName
      `Prelude.hashWithSalt` destination

instance Prelude.NFData DatasetContentDeliveryRule where
  rnf DatasetContentDeliveryRule' {..} =
    Prelude.rnf entryName
      `Prelude.seq` Prelude.rnf destination

instance Data.ToJSON DatasetContentDeliveryRule where
  toJSON DatasetContentDeliveryRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("entryName" Data..=) Prelude.<$> entryName,
            Prelude.Just ("destination" Data..= destination)
          ]
      )
