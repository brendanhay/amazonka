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
-- Module      : Amazonka.MediaPackageV2.Types.ScteHls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageV2.Types.ScteHls where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageV2.Types.AdMarkerHls
import qualified Amazonka.Prelude as Prelude

-- | The SCTE configuration.
--
-- /See:/ 'newScteHls' smart constructor.
data ScteHls = ScteHls'
  { -- | Ad markers indicate when ads should be inserted during playback. If you
    -- include ad markers in the content stream in your upstream encoders, then
    -- you need to inform MediaPackage what to do with the ad markers in the
    -- output. Choose what you want MediaPackage to do with the ad markers.
    --
    -- Value description:
    --
    -- -   DATERANGE - Insert EXT-X-DATERANGE tags to signal ad and program
    --     transition events in TS and CMAF manifests. If you use DATERANGE,
    --     you must set a programDateTimeIntervalSeconds value of 1 or higher.
    --     To learn more about DATERANGE, see
    --     <http://docs.aws.amazon.com/mediapackage/latest/ug/scte-35-ad-marker-ext-x-daterange.html SCTE-35 Ad Marker EXT-X-DATERANGE>.
    adMarkerHls :: Prelude.Maybe AdMarkerHls
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScteHls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adMarkerHls', 'scteHls_adMarkerHls' - Ad markers indicate when ads should be inserted during playback. If you
-- include ad markers in the content stream in your upstream encoders, then
-- you need to inform MediaPackage what to do with the ad markers in the
-- output. Choose what you want MediaPackage to do with the ad markers.
--
-- Value description:
--
-- -   DATERANGE - Insert EXT-X-DATERANGE tags to signal ad and program
--     transition events in TS and CMAF manifests. If you use DATERANGE,
--     you must set a programDateTimeIntervalSeconds value of 1 or higher.
--     To learn more about DATERANGE, see
--     <http://docs.aws.amazon.com/mediapackage/latest/ug/scte-35-ad-marker-ext-x-daterange.html SCTE-35 Ad Marker EXT-X-DATERANGE>.
newScteHls ::
  ScteHls
newScteHls = ScteHls' {adMarkerHls = Prelude.Nothing}

-- | Ad markers indicate when ads should be inserted during playback. If you
-- include ad markers in the content stream in your upstream encoders, then
-- you need to inform MediaPackage what to do with the ad markers in the
-- output. Choose what you want MediaPackage to do with the ad markers.
--
-- Value description:
--
-- -   DATERANGE - Insert EXT-X-DATERANGE tags to signal ad and program
--     transition events in TS and CMAF manifests. If you use DATERANGE,
--     you must set a programDateTimeIntervalSeconds value of 1 or higher.
--     To learn more about DATERANGE, see
--     <http://docs.aws.amazon.com/mediapackage/latest/ug/scte-35-ad-marker-ext-x-daterange.html SCTE-35 Ad Marker EXT-X-DATERANGE>.
scteHls_adMarkerHls :: Lens.Lens' ScteHls (Prelude.Maybe AdMarkerHls)
scteHls_adMarkerHls = Lens.lens (\ScteHls' {adMarkerHls} -> adMarkerHls) (\s@ScteHls' {} a -> s {adMarkerHls = a} :: ScteHls)

instance Data.FromJSON ScteHls where
  parseJSON =
    Data.withObject
      "ScteHls"
      ( \x ->
          ScteHls' Prelude.<$> (x Data..:? "AdMarkerHls")
      )

instance Prelude.Hashable ScteHls where
  hashWithSalt _salt ScteHls' {..} =
    _salt `Prelude.hashWithSalt` adMarkerHls

instance Prelude.NFData ScteHls where
  rnf ScteHls' {..} = Prelude.rnf adMarkerHls

instance Data.ToJSON ScteHls where
  toJSON ScteHls' {..} =
    Data.object
      ( Prelude.catMaybes
          [("AdMarkerHls" Data..=) Prelude.<$> adMarkerHls]
      )
