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
-- Module      : Amazonka.MediaConvert.Types.Hdr10Plus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.Hdr10Plus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Setting for HDR10+ metadata insertion
--
-- /See:/ 'newHdr10Plus' smart constructor.
data Hdr10Plus = Hdr10Plus'
  { -- | Specify the HDR10+ mastering display normalized peak luminance, in nits.
    -- This is the normalized actual peak luminance of the mastering display,
    -- as defined by ST 2094-40.
    masteringMonitorNits :: Prelude.Maybe Prelude.Natural,
    -- | Specify the HDR10+ target display nominal peak luminance, in nits. This
    -- is the nominal maximum luminance of the target display as defined by ST
    -- 2094-40.
    targetMonitorNits :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Hdr10Plus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'masteringMonitorNits', 'hdr10Plus_masteringMonitorNits' - Specify the HDR10+ mastering display normalized peak luminance, in nits.
-- This is the normalized actual peak luminance of the mastering display,
-- as defined by ST 2094-40.
--
-- 'targetMonitorNits', 'hdr10Plus_targetMonitorNits' - Specify the HDR10+ target display nominal peak luminance, in nits. This
-- is the nominal maximum luminance of the target display as defined by ST
-- 2094-40.
newHdr10Plus ::
  Hdr10Plus
newHdr10Plus =
  Hdr10Plus'
    { masteringMonitorNits = Prelude.Nothing,
      targetMonitorNits = Prelude.Nothing
    }

-- | Specify the HDR10+ mastering display normalized peak luminance, in nits.
-- This is the normalized actual peak luminance of the mastering display,
-- as defined by ST 2094-40.
hdr10Plus_masteringMonitorNits :: Lens.Lens' Hdr10Plus (Prelude.Maybe Prelude.Natural)
hdr10Plus_masteringMonitorNits = Lens.lens (\Hdr10Plus' {masteringMonitorNits} -> masteringMonitorNits) (\s@Hdr10Plus' {} a -> s {masteringMonitorNits = a} :: Hdr10Plus)

-- | Specify the HDR10+ target display nominal peak luminance, in nits. This
-- is the nominal maximum luminance of the target display as defined by ST
-- 2094-40.
hdr10Plus_targetMonitorNits :: Lens.Lens' Hdr10Plus (Prelude.Maybe Prelude.Natural)
hdr10Plus_targetMonitorNits = Lens.lens (\Hdr10Plus' {targetMonitorNits} -> targetMonitorNits) (\s@Hdr10Plus' {} a -> s {targetMonitorNits = a} :: Hdr10Plus)

instance Data.FromJSON Hdr10Plus where
  parseJSON =
    Data.withObject
      "Hdr10Plus"
      ( \x ->
          Hdr10Plus'
            Prelude.<$> (x Data..:? "masteringMonitorNits")
            Prelude.<*> (x Data..:? "targetMonitorNits")
      )

instance Prelude.Hashable Hdr10Plus where
  hashWithSalt _salt Hdr10Plus' {..} =
    _salt `Prelude.hashWithSalt` masteringMonitorNits
      `Prelude.hashWithSalt` targetMonitorNits

instance Prelude.NFData Hdr10Plus where
  rnf Hdr10Plus' {..} =
    Prelude.rnf masteringMonitorNits
      `Prelude.seq` Prelude.rnf targetMonitorNits

instance Data.ToJSON Hdr10Plus where
  toJSON Hdr10Plus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("masteringMonitorNits" Data..=)
              Prelude.<$> masteringMonitorNits,
            ("targetMonitorNits" Data..=)
              Prelude.<$> targetMonitorNits
          ]
      )
