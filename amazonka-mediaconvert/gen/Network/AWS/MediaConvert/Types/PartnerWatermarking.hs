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
-- Module      : Network.AWS.MediaConvert.Types.PartnerWatermarking
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.PartnerWatermarking where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.NexGuardFileMarkerSettings
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON PartnerWatermarking where
  parseJSON =
    Core.withObject
      "PartnerWatermarking"
      ( \x ->
          PartnerWatermarking'
            Prelude.<$> (x Core..:? "nexguardFileMarkerSettings")
      )

instance Prelude.Hashable PartnerWatermarking

instance Prelude.NFData PartnerWatermarking

instance Core.ToJSON PartnerWatermarking where
  toJSON PartnerWatermarking' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nexguardFileMarkerSettings" Core..=)
              Prelude.<$> nexguardFileMarkerSettings
          ]
      )
