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
-- Module      : Amazonka.QuickSight.Types.DefaultSectionBasedLayoutConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DefaultSectionBasedLayoutConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SectionBasedLayoutCanvasSizeOptions

-- | The options that determine the default settings for a section-based
-- layout configuration.
--
-- /See:/ 'newDefaultSectionBasedLayoutConfiguration' smart constructor.
data DefaultSectionBasedLayoutConfiguration = DefaultSectionBasedLayoutConfiguration'
  { -- | Determines the screen canvas size options for a section-based layout.
    canvasSizeOptions :: SectionBasedLayoutCanvasSizeOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultSectionBasedLayoutConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'canvasSizeOptions', 'defaultSectionBasedLayoutConfiguration_canvasSizeOptions' - Determines the screen canvas size options for a section-based layout.
newDefaultSectionBasedLayoutConfiguration ::
  -- | 'canvasSizeOptions'
  SectionBasedLayoutCanvasSizeOptions ->
  DefaultSectionBasedLayoutConfiguration
newDefaultSectionBasedLayoutConfiguration
  pCanvasSizeOptions_ =
    DefaultSectionBasedLayoutConfiguration'
      { canvasSizeOptions =
          pCanvasSizeOptions_
      }

-- | Determines the screen canvas size options for a section-based layout.
defaultSectionBasedLayoutConfiguration_canvasSizeOptions :: Lens.Lens' DefaultSectionBasedLayoutConfiguration SectionBasedLayoutCanvasSizeOptions
defaultSectionBasedLayoutConfiguration_canvasSizeOptions = Lens.lens (\DefaultSectionBasedLayoutConfiguration' {canvasSizeOptions} -> canvasSizeOptions) (\s@DefaultSectionBasedLayoutConfiguration' {} a -> s {canvasSizeOptions = a} :: DefaultSectionBasedLayoutConfiguration)

instance
  Data.FromJSON
    DefaultSectionBasedLayoutConfiguration
  where
  parseJSON =
    Data.withObject
      "DefaultSectionBasedLayoutConfiguration"
      ( \x ->
          DefaultSectionBasedLayoutConfiguration'
            Prelude.<$> (x Data..: "CanvasSizeOptions")
      )

instance
  Prelude.Hashable
    DefaultSectionBasedLayoutConfiguration
  where
  hashWithSalt
    _salt
    DefaultSectionBasedLayoutConfiguration' {..} =
      _salt `Prelude.hashWithSalt` canvasSizeOptions

instance
  Prelude.NFData
    DefaultSectionBasedLayoutConfiguration
  where
  rnf DefaultSectionBasedLayoutConfiguration' {..} =
    Prelude.rnf canvasSizeOptions

instance
  Data.ToJSON
    DefaultSectionBasedLayoutConfiguration
  where
  toJSON DefaultSectionBasedLayoutConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CanvasSizeOptions" Data..= canvasSizeOptions)
          ]
      )
