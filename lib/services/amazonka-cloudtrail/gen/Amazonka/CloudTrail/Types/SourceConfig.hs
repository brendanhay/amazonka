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
-- Module      : Amazonka.CloudTrail.Types.SourceConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudTrail.Types.SourceConfig where

import Amazonka.CloudTrail.Types.AdvancedEventSelector
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains configuration information about the channel.
--
-- /See:/ 'newSourceConfig' smart constructor.
data SourceConfig = SourceConfig'
  { -- | Specifies whether the channel applies to a single region or to all
    -- regions.
    applyToAllRegions :: Prelude.Maybe Prelude.Bool,
    -- | The advanced event selectors that are configured for the channel.
    advancedEventSelectors :: Prelude.Maybe [AdvancedEventSelector]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyToAllRegions', 'sourceConfig_applyToAllRegions' - Specifies whether the channel applies to a single region or to all
-- regions.
--
-- 'advancedEventSelectors', 'sourceConfig_advancedEventSelectors' - The advanced event selectors that are configured for the channel.
newSourceConfig ::
  SourceConfig
newSourceConfig =
  SourceConfig'
    { applyToAllRegions = Prelude.Nothing,
      advancedEventSelectors = Prelude.Nothing
    }

-- | Specifies whether the channel applies to a single region or to all
-- regions.
sourceConfig_applyToAllRegions :: Lens.Lens' SourceConfig (Prelude.Maybe Prelude.Bool)
sourceConfig_applyToAllRegions = Lens.lens (\SourceConfig' {applyToAllRegions} -> applyToAllRegions) (\s@SourceConfig' {} a -> s {applyToAllRegions = a} :: SourceConfig)

-- | The advanced event selectors that are configured for the channel.
sourceConfig_advancedEventSelectors :: Lens.Lens' SourceConfig (Prelude.Maybe [AdvancedEventSelector])
sourceConfig_advancedEventSelectors = Lens.lens (\SourceConfig' {advancedEventSelectors} -> advancedEventSelectors) (\s@SourceConfig' {} a -> s {advancedEventSelectors = a} :: SourceConfig) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SourceConfig where
  parseJSON =
    Core.withObject
      "SourceConfig"
      ( \x ->
          SourceConfig'
            Prelude.<$> (x Core..:? "ApplyToAllRegions")
            Prelude.<*> ( x Core..:? "AdvancedEventSelectors"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SourceConfig where
  hashWithSalt _salt SourceConfig' {..} =
    _salt `Prelude.hashWithSalt` applyToAllRegions
      `Prelude.hashWithSalt` advancedEventSelectors

instance Prelude.NFData SourceConfig where
  rnf SourceConfig' {..} =
    Prelude.rnf applyToAllRegions
      `Prelude.seq` Prelude.rnf advancedEventSelectors
