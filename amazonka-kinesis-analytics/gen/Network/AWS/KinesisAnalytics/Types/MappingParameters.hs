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
-- Module      : Network.AWS.KinesisAnalytics.Types.MappingParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.MappingParameters where

import qualified Network.AWS.Core as Core
import Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
import Network.AWS.KinesisAnalytics.Types.JSONMappingParameters
import qualified Network.AWS.Lens as Lens

-- | When configuring application input at the time of creating or updating
-- an application, provides additional mapping information specific to the
-- record format (such as JSON, CSV, or record fields delimited by some
-- delimiter) on the streaming source.
--
-- /See:/ 'newMappingParameters' smart constructor.
data MappingParameters = MappingParameters'
  { -- | Provides additional mapping information when JSON is the record format
    -- on the streaming source.
    jSONMappingParameters :: Core.Maybe JSONMappingParameters,
    -- | Provides additional mapping information when the record format uses
    -- delimiters (for example, CSV).
    cSVMappingParameters :: Core.Maybe CSVMappingParameters
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MappingParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jSONMappingParameters', 'mappingParameters_jSONMappingParameters' - Provides additional mapping information when JSON is the record format
-- on the streaming source.
--
-- 'cSVMappingParameters', 'mappingParameters_cSVMappingParameters' - Provides additional mapping information when the record format uses
-- delimiters (for example, CSV).
newMappingParameters ::
  MappingParameters
newMappingParameters =
  MappingParameters'
    { jSONMappingParameters =
        Core.Nothing,
      cSVMappingParameters = Core.Nothing
    }

-- | Provides additional mapping information when JSON is the record format
-- on the streaming source.
mappingParameters_jSONMappingParameters :: Lens.Lens' MappingParameters (Core.Maybe JSONMappingParameters)
mappingParameters_jSONMappingParameters = Lens.lens (\MappingParameters' {jSONMappingParameters} -> jSONMappingParameters) (\s@MappingParameters' {} a -> s {jSONMappingParameters = a} :: MappingParameters)

-- | Provides additional mapping information when the record format uses
-- delimiters (for example, CSV).
mappingParameters_cSVMappingParameters :: Lens.Lens' MappingParameters (Core.Maybe CSVMappingParameters)
mappingParameters_cSVMappingParameters = Lens.lens (\MappingParameters' {cSVMappingParameters} -> cSVMappingParameters) (\s@MappingParameters' {} a -> s {cSVMappingParameters = a} :: MappingParameters)

instance Core.FromJSON MappingParameters where
  parseJSON =
    Core.withObject
      "MappingParameters"
      ( \x ->
          MappingParameters'
            Core.<$> (x Core..:? "JSONMappingParameters")
            Core.<*> (x Core..:? "CSVMappingParameters")
      )

instance Core.Hashable MappingParameters

instance Core.NFData MappingParameters

instance Core.ToJSON MappingParameters where
  toJSON MappingParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("JSONMappingParameters" Core..=)
              Core.<$> jSONMappingParameters,
            ("CSVMappingParameters" Core..=)
              Core.<$> cSVMappingParameters
          ]
      )
