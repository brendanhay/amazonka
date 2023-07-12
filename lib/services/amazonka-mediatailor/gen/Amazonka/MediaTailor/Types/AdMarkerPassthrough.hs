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
-- Module      : Amazonka.MediaTailor.Types.AdMarkerPassthrough
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaTailor.Types.AdMarkerPassthrough where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | For HLS, when set to @true@, MediaTailor passes through @EXT-X-CUE-IN@,
-- @EXT-X-CUE-OUT@, and @EXT-X-SPLICEPOINT-SCTE35@ ad markers from the
-- origin manifest to the MediaTailor personalized manifest.
--
-- No logic is applied to these ad markers. For example, if @EXT-X-CUE-OUT@
-- has a value of @60@, but no ads are filled for that ad break,
-- MediaTailor will not set the value to @0@.
--
-- /See:/ 'newAdMarkerPassthrough' smart constructor.
data AdMarkerPassthrough = AdMarkerPassthrough'
  { -- | Enables ad marker passthrough for your configuration.
    enabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AdMarkerPassthrough' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'adMarkerPassthrough_enabled' - Enables ad marker passthrough for your configuration.
newAdMarkerPassthrough ::
  AdMarkerPassthrough
newAdMarkerPassthrough =
  AdMarkerPassthrough' {enabled = Prelude.Nothing}

-- | Enables ad marker passthrough for your configuration.
adMarkerPassthrough_enabled :: Lens.Lens' AdMarkerPassthrough (Prelude.Maybe Prelude.Bool)
adMarkerPassthrough_enabled = Lens.lens (\AdMarkerPassthrough' {enabled} -> enabled) (\s@AdMarkerPassthrough' {} a -> s {enabled = a} :: AdMarkerPassthrough)

instance Data.FromJSON AdMarkerPassthrough where
  parseJSON =
    Data.withObject
      "AdMarkerPassthrough"
      ( \x ->
          AdMarkerPassthrough'
            Prelude.<$> (x Data..:? "Enabled")
      )

instance Prelude.Hashable AdMarkerPassthrough where
  hashWithSalt _salt AdMarkerPassthrough' {..} =
    _salt `Prelude.hashWithSalt` enabled

instance Prelude.NFData AdMarkerPassthrough where
  rnf AdMarkerPassthrough' {..} = Prelude.rnf enabled

instance Data.ToJSON AdMarkerPassthrough where
  toJSON AdMarkerPassthrough' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Enabled" Data..=) Prelude.<$> enabled]
      )
