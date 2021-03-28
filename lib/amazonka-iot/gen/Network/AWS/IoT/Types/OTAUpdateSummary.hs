{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OTAUpdateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.OTAUpdateSummary
  ( OTAUpdateSummary (..)
  -- * Smart constructor
  , mkOTAUpdateSummary
  -- * Lenses
  , otausCreationDate
  , otausOtaUpdateArn
  , otausOtaUpdateId
  ) where

import qualified Network.AWS.IoT.Types.OtaUpdateArn as Types
import qualified Network.AWS.IoT.Types.OtaUpdateId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An OTA update summary.
--
-- /See:/ 'mkOTAUpdateSummary' smart constructor.
data OTAUpdateSummary = OTAUpdateSummary'
  { creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the OTA update was created.
  , otaUpdateArn :: Core.Maybe Types.OtaUpdateArn
    -- ^ The OTA update ARN.
  , otaUpdateId :: Core.Maybe Types.OtaUpdateId
    -- ^ The OTA update ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'OTAUpdateSummary' value with any optional fields omitted.
mkOTAUpdateSummary
    :: OTAUpdateSummary
mkOTAUpdateSummary
  = OTAUpdateSummary'{creationDate = Core.Nothing,
                      otaUpdateArn = Core.Nothing, otaUpdateId = Core.Nothing}

-- | The date when the OTA update was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otausCreationDate :: Lens.Lens' OTAUpdateSummary (Core.Maybe Core.NominalDiffTime)
otausCreationDate = Lens.field @"creationDate"
{-# INLINEABLE otausCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | The OTA update ARN.
--
-- /Note:/ Consider using 'otaUpdateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otausOtaUpdateArn :: Lens.Lens' OTAUpdateSummary (Core.Maybe Types.OtaUpdateArn)
otausOtaUpdateArn = Lens.field @"otaUpdateArn"
{-# INLINEABLE otausOtaUpdateArn #-}
{-# DEPRECATED otaUpdateArn "Use generic-lens or generic-optics with 'otaUpdateArn' instead"  #-}

-- | The OTA update ID.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otausOtaUpdateId :: Lens.Lens' OTAUpdateSummary (Core.Maybe Types.OtaUpdateId)
otausOtaUpdateId = Lens.field @"otaUpdateId"
{-# INLINEABLE otausOtaUpdateId #-}
{-# DEPRECATED otaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead"  #-}

instance Core.FromJSON OTAUpdateSummary where
        parseJSON
          = Core.withObject "OTAUpdateSummary" Core.$
              \ x ->
                OTAUpdateSummary' Core.<$>
                  (x Core..:? "creationDate") Core.<*> x Core..:? "otaUpdateArn"
                    Core.<*> x Core..:? "otaUpdateId"
