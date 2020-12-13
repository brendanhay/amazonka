{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OTAUpdateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OTAUpdateSummary
  ( OTAUpdateSummary (..),

    -- * Smart constructor
    mkOTAUpdateSummary,

    -- * Lenses
    otausCreationDate,
    otausOtaUpdateId,
    otausOtaUpdateARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An OTA update summary.
--
-- /See:/ 'mkOTAUpdateSummary' smart constructor.
data OTAUpdateSummary = OTAUpdateSummary'
  { -- | The date when the OTA update was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The OTA update ID.
    otaUpdateId :: Lude.Maybe Lude.Text,
    -- | The OTA update ARN.
    otaUpdateARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OTAUpdateSummary' with the minimum fields required to make a request.
--
-- * 'creationDate' - The date when the OTA update was created.
-- * 'otaUpdateId' - The OTA update ID.
-- * 'otaUpdateARN' - The OTA update ARN.
mkOTAUpdateSummary ::
  OTAUpdateSummary
mkOTAUpdateSummary =
  OTAUpdateSummary'
    { creationDate = Lude.Nothing,
      otaUpdateId = Lude.Nothing,
      otaUpdateARN = Lude.Nothing
    }

-- | The date when the OTA update was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otausCreationDate :: Lens.Lens' OTAUpdateSummary (Lude.Maybe Lude.Timestamp)
otausCreationDate = Lens.lens (creationDate :: OTAUpdateSummary -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: OTAUpdateSummary)
{-# DEPRECATED otausCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The OTA update ID.
--
-- /Note:/ Consider using 'otaUpdateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otausOtaUpdateId :: Lens.Lens' OTAUpdateSummary (Lude.Maybe Lude.Text)
otausOtaUpdateId = Lens.lens (otaUpdateId :: OTAUpdateSummary -> Lude.Maybe Lude.Text) (\s a -> s {otaUpdateId = a} :: OTAUpdateSummary)
{-# DEPRECATED otausOtaUpdateId "Use generic-lens or generic-optics with 'otaUpdateId' instead." #-}

-- | The OTA update ARN.
--
-- /Note:/ Consider using 'otaUpdateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
otausOtaUpdateARN :: Lens.Lens' OTAUpdateSummary (Lude.Maybe Lude.Text)
otausOtaUpdateARN = Lens.lens (otaUpdateARN :: OTAUpdateSummary -> Lude.Maybe Lude.Text) (\s a -> s {otaUpdateARN = a} :: OTAUpdateSummary)
{-# DEPRECATED otausOtaUpdateARN "Use generic-lens or generic-optics with 'otaUpdateARN' instead." #-}

instance Lude.FromJSON OTAUpdateSummary where
  parseJSON =
    Lude.withObject
      "OTAUpdateSummary"
      ( \x ->
          OTAUpdateSummary'
            Lude.<$> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "otaUpdateId")
            Lude.<*> (x Lude..:? "otaUpdateArn")
      )
