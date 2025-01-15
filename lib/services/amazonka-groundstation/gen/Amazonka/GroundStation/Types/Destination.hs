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
-- Module      : Amazonka.GroundStation.Types.Destination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.Destination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.ConfigCapabilityType
import Amazonka.GroundStation.Types.ConfigDetails
import qualified Amazonka.Prelude as Prelude

-- | Dataflow details for the destination side.
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | Additional details for a @Config@, if type is dataflow endpoint or
    -- antenna demod decode.
    configDetails :: Prelude.Maybe ConfigDetails,
    -- | UUID of a @Config@.
    configId :: Prelude.Maybe Prelude.Text,
    -- | Type of a @Config@.
    configType :: Prelude.Maybe ConfigCapabilityType,
    -- | Region of a dataflow destination.
    dataflowDestinationRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configDetails', 'destination_configDetails' - Additional details for a @Config@, if type is dataflow endpoint or
-- antenna demod decode.
--
-- 'configId', 'destination_configId' - UUID of a @Config@.
--
-- 'configType', 'destination_configType' - Type of a @Config@.
--
-- 'dataflowDestinationRegion', 'destination_dataflowDestinationRegion' - Region of a dataflow destination.
newDestination ::
  Destination
newDestination =
  Destination'
    { configDetails = Prelude.Nothing,
      configId = Prelude.Nothing,
      configType = Prelude.Nothing,
      dataflowDestinationRegion = Prelude.Nothing
    }

-- | Additional details for a @Config@, if type is dataflow endpoint or
-- antenna demod decode.
destination_configDetails :: Lens.Lens' Destination (Prelude.Maybe ConfigDetails)
destination_configDetails = Lens.lens (\Destination' {configDetails} -> configDetails) (\s@Destination' {} a -> s {configDetails = a} :: Destination)

-- | UUID of a @Config@.
destination_configId :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_configId = Lens.lens (\Destination' {configId} -> configId) (\s@Destination' {} a -> s {configId = a} :: Destination)

-- | Type of a @Config@.
destination_configType :: Lens.Lens' Destination (Prelude.Maybe ConfigCapabilityType)
destination_configType = Lens.lens (\Destination' {configType} -> configType) (\s@Destination' {} a -> s {configType = a} :: Destination)

-- | Region of a dataflow destination.
destination_dataflowDestinationRegion :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_dataflowDestinationRegion = Lens.lens (\Destination' {dataflowDestinationRegion} -> dataflowDestinationRegion) (\s@Destination' {} a -> s {dataflowDestinationRegion = a} :: Destination)

instance Data.FromJSON Destination where
  parseJSON =
    Data.withObject
      "Destination"
      ( \x ->
          Destination'
            Prelude.<$> (x Data..:? "configDetails")
            Prelude.<*> (x Data..:? "configId")
            Prelude.<*> (x Data..:? "configType")
            Prelude.<*> (x Data..:? "dataflowDestinationRegion")
      )

instance Prelude.Hashable Destination where
  hashWithSalt _salt Destination' {..} =
    _salt
      `Prelude.hashWithSalt` configDetails
      `Prelude.hashWithSalt` configId
      `Prelude.hashWithSalt` configType
      `Prelude.hashWithSalt` dataflowDestinationRegion

instance Prelude.NFData Destination where
  rnf Destination' {..} =
    Prelude.rnf configDetails `Prelude.seq`
      Prelude.rnf configId `Prelude.seq`
        Prelude.rnf configType `Prelude.seq`
          Prelude.rnf dataflowDestinationRegion
