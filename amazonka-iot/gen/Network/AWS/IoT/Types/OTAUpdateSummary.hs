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
-- Module      : Network.AWS.IoT.Types.OTAUpdateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OTAUpdateSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An OTA update summary.
--
-- /See:/ 'newOTAUpdateSummary' smart constructor.
data OTAUpdateSummary = OTAUpdateSummary'
  { -- | The OTA update ARN.
    otaUpdateArn :: Core.Maybe Core.Text,
    -- | The date when the OTA update was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The OTA update ID.
    otaUpdateId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OTAUpdateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otaUpdateArn', 'oTAUpdateSummary_otaUpdateArn' - The OTA update ARN.
--
-- 'creationDate', 'oTAUpdateSummary_creationDate' - The date when the OTA update was created.
--
-- 'otaUpdateId', 'oTAUpdateSummary_otaUpdateId' - The OTA update ID.
newOTAUpdateSummary ::
  OTAUpdateSummary
newOTAUpdateSummary =
  OTAUpdateSummary'
    { otaUpdateArn = Core.Nothing,
      creationDate = Core.Nothing,
      otaUpdateId = Core.Nothing
    }

-- | The OTA update ARN.
oTAUpdateSummary_otaUpdateArn :: Lens.Lens' OTAUpdateSummary (Core.Maybe Core.Text)
oTAUpdateSummary_otaUpdateArn = Lens.lens (\OTAUpdateSummary' {otaUpdateArn} -> otaUpdateArn) (\s@OTAUpdateSummary' {} a -> s {otaUpdateArn = a} :: OTAUpdateSummary)

-- | The date when the OTA update was created.
oTAUpdateSummary_creationDate :: Lens.Lens' OTAUpdateSummary (Core.Maybe Core.UTCTime)
oTAUpdateSummary_creationDate = Lens.lens (\OTAUpdateSummary' {creationDate} -> creationDate) (\s@OTAUpdateSummary' {} a -> s {creationDate = a} :: OTAUpdateSummary) Core.. Lens.mapping Core._Time

-- | The OTA update ID.
oTAUpdateSummary_otaUpdateId :: Lens.Lens' OTAUpdateSummary (Core.Maybe Core.Text)
oTAUpdateSummary_otaUpdateId = Lens.lens (\OTAUpdateSummary' {otaUpdateId} -> otaUpdateId) (\s@OTAUpdateSummary' {} a -> s {otaUpdateId = a} :: OTAUpdateSummary)

instance Core.FromJSON OTAUpdateSummary where
  parseJSON =
    Core.withObject
      "OTAUpdateSummary"
      ( \x ->
          OTAUpdateSummary'
            Core.<$> (x Core..:? "otaUpdateArn")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "otaUpdateId")
      )

instance Core.Hashable OTAUpdateSummary

instance Core.NFData OTAUpdateSummary
