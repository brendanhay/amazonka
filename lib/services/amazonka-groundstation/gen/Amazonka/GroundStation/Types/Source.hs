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
-- Module      : Amazonka.GroundStation.Types.Source
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.Source where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.ConfigCapabilityType
import Amazonka.GroundStation.Types.ConfigDetails
import qualified Amazonka.Prelude as Prelude

-- | Dataflow details for the source side.
--
-- /See:/ 'newSource' smart constructor.
data Source = Source'
  { -- | Additional details for a @Config@, if type is @dataflow-endpoint@ or
    -- @antenna-downlink-demod-decode@
    configDetails :: Prelude.Maybe ConfigDetails,
    -- | UUID of a @Config@.
    configId :: Prelude.Maybe Prelude.Text,
    -- | Type of a @Config@.
    configType :: Prelude.Maybe ConfigCapabilityType,
    -- | Region of a dataflow source.
    dataflowSourceRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Source' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configDetails', 'source_configDetails' - Additional details for a @Config@, if type is @dataflow-endpoint@ or
-- @antenna-downlink-demod-decode@
--
-- 'configId', 'source_configId' - UUID of a @Config@.
--
-- 'configType', 'source_configType' - Type of a @Config@.
--
-- 'dataflowSourceRegion', 'source_dataflowSourceRegion' - Region of a dataflow source.
newSource ::
  Source
newSource =
  Source'
    { configDetails = Prelude.Nothing,
      configId = Prelude.Nothing,
      configType = Prelude.Nothing,
      dataflowSourceRegion = Prelude.Nothing
    }

-- | Additional details for a @Config@, if type is @dataflow-endpoint@ or
-- @antenna-downlink-demod-decode@
source_configDetails :: Lens.Lens' Source (Prelude.Maybe ConfigDetails)
source_configDetails = Lens.lens (\Source' {configDetails} -> configDetails) (\s@Source' {} a -> s {configDetails = a} :: Source)

-- | UUID of a @Config@.
source_configId :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_configId = Lens.lens (\Source' {configId} -> configId) (\s@Source' {} a -> s {configId = a} :: Source)

-- | Type of a @Config@.
source_configType :: Lens.Lens' Source (Prelude.Maybe ConfigCapabilityType)
source_configType = Lens.lens (\Source' {configType} -> configType) (\s@Source' {} a -> s {configType = a} :: Source)

-- | Region of a dataflow source.
source_dataflowSourceRegion :: Lens.Lens' Source (Prelude.Maybe Prelude.Text)
source_dataflowSourceRegion = Lens.lens (\Source' {dataflowSourceRegion} -> dataflowSourceRegion) (\s@Source' {} a -> s {dataflowSourceRegion = a} :: Source)

instance Data.FromJSON Source where
  parseJSON =
    Data.withObject
      "Source"
      ( \x ->
          Source'
            Prelude.<$> (x Data..:? "configDetails")
            Prelude.<*> (x Data..:? "configId")
            Prelude.<*> (x Data..:? "configType")
            Prelude.<*> (x Data..:? "dataflowSourceRegion")
      )

instance Prelude.Hashable Source where
  hashWithSalt _salt Source' {..} =
    _salt `Prelude.hashWithSalt` configDetails
      `Prelude.hashWithSalt` configId
      `Prelude.hashWithSalt` configType
      `Prelude.hashWithSalt` dataflowSourceRegion

instance Prelude.NFData Source where
  rnf Source' {..} =
    Prelude.rnf configDetails
      `Prelude.seq` Prelude.rnf configId
      `Prelude.seq` Prelude.rnf configType
      `Prelude.seq` Prelude.rnf dataflowSourceRegion
