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
-- Module      : Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryRule where

import Network.AWS.IoTAnalytics.Types.DatasetContentDeliveryDestination
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON DatasetContentDeliveryRule where
  parseJSON =
    Prelude.withObject
      "DatasetContentDeliveryRule"
      ( \x ->
          DatasetContentDeliveryRule'
            Prelude.<$> (x Prelude..:? "entryName")
            Prelude.<*> (x Prelude..: "destination")
      )

instance Prelude.Hashable DatasetContentDeliveryRule

instance Prelude.NFData DatasetContentDeliveryRule

instance Prelude.ToJSON DatasetContentDeliveryRule where
  toJSON DatasetContentDeliveryRule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("entryName" Prelude..=) Prelude.<$> entryName,
            Prelude.Just ("destination" Prelude..= destination)
          ]
      )
