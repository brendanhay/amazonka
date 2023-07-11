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
-- Module      : Amazonka.MediaConvert.Types.PartnerWatermarking
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.PartnerWatermarking where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.NexGuardFileMarkerSettings
import qualified Amazonka.Prelude as Prelude

-- | If you work with a third party video watermarking partner, use the group
-- of settings that correspond with your watermarking partner to include
-- watermarks in your output.
--
-- /See:/ 'newPartnerWatermarking' smart constructor.
data PartnerWatermarking = PartnerWatermarking'
  { -- | For forensic video watermarking, MediaConvert supports Nagra NexGuard
    -- File Marker watermarking. MediaConvert supports both PreRelease Content
    -- (NGPR\/G2) and OTT Streaming workflows.
    nexguardFileMarkerSettings :: Prelude.Maybe NexGuardFileMarkerSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartnerWatermarking' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nexguardFileMarkerSettings', 'partnerWatermarking_nexguardFileMarkerSettings' - For forensic video watermarking, MediaConvert supports Nagra NexGuard
-- File Marker watermarking. MediaConvert supports both PreRelease Content
-- (NGPR\/G2) and OTT Streaming workflows.
newPartnerWatermarking ::
  PartnerWatermarking
newPartnerWatermarking =
  PartnerWatermarking'
    { nexguardFileMarkerSettings =
        Prelude.Nothing
    }

-- | For forensic video watermarking, MediaConvert supports Nagra NexGuard
-- File Marker watermarking. MediaConvert supports both PreRelease Content
-- (NGPR\/G2) and OTT Streaming workflows.
partnerWatermarking_nexguardFileMarkerSettings :: Lens.Lens' PartnerWatermarking (Prelude.Maybe NexGuardFileMarkerSettings)
partnerWatermarking_nexguardFileMarkerSettings = Lens.lens (\PartnerWatermarking' {nexguardFileMarkerSettings} -> nexguardFileMarkerSettings) (\s@PartnerWatermarking' {} a -> s {nexguardFileMarkerSettings = a} :: PartnerWatermarking)

instance Data.FromJSON PartnerWatermarking where
  parseJSON =
    Data.withObject
      "PartnerWatermarking"
      ( \x ->
          PartnerWatermarking'
            Prelude.<$> (x Data..:? "nexguardFileMarkerSettings")
      )

instance Prelude.Hashable PartnerWatermarking where
  hashWithSalt _salt PartnerWatermarking' {..} =
    _salt
      `Prelude.hashWithSalt` nexguardFileMarkerSettings

instance Prelude.NFData PartnerWatermarking where
  rnf PartnerWatermarking' {..} =
    Prelude.rnf nexguardFileMarkerSettings

instance Data.ToJSON PartnerWatermarking where
  toJSON PartnerWatermarking' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nexguardFileMarkerSettings" Data..=)
              Prelude.<$> nexguardFileMarkerSettings
          ]
      )
