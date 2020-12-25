{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice
  ( PublicWorkforceTaskPrice (..),

    -- * Smart constructor
    mkPublicWorkforceTaskPrice,

    -- * Lenses
    pwtpAmountInUsd,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.USD as Types

-- | Defines the amount of money paid to an Amazon Mechanical Turk worker for each task performed.
--
-- Use one of the following prices for bounding box tasks. Prices are in US dollars and should be based on the complexity of the task; the longer it takes in your initial testing, the more you should offer.
--
--     * 0.036
--
--
--     * 0.048
--
--
--     * 0.060
--
--
--     * 0.072
--
--
--     * 0.120
--
--
--     * 0.240
--
--
--     * 0.360
--
--
--     * 0.480
--
--
--     * 0.600
--
--
--     * 0.720
--
--
--     * 0.840
--
--
--     * 0.960
--
--
--     * 1.080
--
--
--     * 1.200
--
--
-- Use one of the following prices for image classification, text classification, and custom tasks. Prices are in US dollars.
--
--     * 0.012
--
--
--     * 0.024
--
--
--     * 0.036
--
--
--     * 0.048
--
--
--     * 0.060
--
--
--     * 0.072
--
--
--     * 0.120
--
--
--     * 0.240
--
--
--     * 0.360
--
--
--     * 0.480
--
--
--     * 0.600
--
--
--     * 0.720
--
--
--     * 0.840
--
--
--     * 0.960
--
--
--     * 1.080
--
--
--     * 1.200
--
--
-- Use one of the following prices for semantic segmentation tasks. Prices are in US dollars.
--
--     * 0.840
--
--
--     * 0.960
--
--
--     * 1.080
--
--
--     * 1.200
--
--
-- Use one of the following prices for Textract AnalyzeDocument Important Form Key Amazon Augmented AI review tasks. Prices are in US dollars.
--
--     * 2.400
--
--
--     * 2.280
--
--
--     * 2.160
--
--
--     * 2.040
--
--
--     * 1.920
--
--
--     * 1.800
--
--
--     * 1.680
--
--
--     * 1.560
--
--
--     * 1.440
--
--
--     * 1.320
--
--
--     * 1.200
--
--
--     * 1.080
--
--
--     * 0.960
--
--
--     * 0.840
--
--
--     * 0.720
--
--
--     * 0.600
--
--
--     * 0.480
--
--
--     * 0.360
--
--
--     * 0.240
--
--
--     * 0.120
--
--
--     * 0.072
--
--
--     * 0.060
--
--
--     * 0.048
--
--
--     * 0.036
--
--
--     * 0.024
--
--
--     * 0.012
--
--
-- Use one of the following prices for Rekognition DetectModerationLabels Amazon Augmented AI review tasks. Prices are in US dollars.
--
--     * 1.200
--
--
--     * 1.080
--
--
--     * 0.960
--
--
--     * 0.840
--
--
--     * 0.720
--
--
--     * 0.600
--
--
--     * 0.480
--
--
--     * 0.360
--
--
--     * 0.240
--
--
--     * 0.120
--
--
--     * 0.072
--
--
--     * 0.060
--
--
--     * 0.048
--
--
--     * 0.036
--
--
--     * 0.024
--
--
--     * 0.012
--
--
-- Use one of the following prices for Amazon Augmented AI custom human review tasks. Prices are in US dollars.
--
--     * 1.200
--
--
--     * 1.080
--
--
--     * 0.960
--
--
--     * 0.840
--
--
--     * 0.720
--
--
--     * 0.600
--
--
--     * 0.480
--
--
--     * 0.360
--
--
--     * 0.240
--
--
--     * 0.120
--
--
--     * 0.072
--
--
--     * 0.060
--
--
--     * 0.048
--
--
--     * 0.036
--
--
--     * 0.024
--
--
--     * 0.012
--
--
--
-- /See:/ 'mkPublicWorkforceTaskPrice' smart constructor.
newtype PublicWorkforceTaskPrice = PublicWorkforceTaskPrice'
  { -- | Defines the amount of money paid to an Amazon Mechanical Turk worker in United States dollars.
    amountInUsd :: Core.Maybe Types.USD
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PublicWorkforceTaskPrice' value with any optional fields omitted.
mkPublicWorkforceTaskPrice ::
  PublicWorkforceTaskPrice
mkPublicWorkforceTaskPrice =
  PublicWorkforceTaskPrice' {amountInUsd = Core.Nothing}

-- | Defines the amount of money paid to an Amazon Mechanical Turk worker in United States dollars.
--
-- /Note:/ Consider using 'amountInUsd' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pwtpAmountInUsd :: Lens.Lens' PublicWorkforceTaskPrice (Core.Maybe Types.USD)
pwtpAmountInUsd = Lens.field @"amountInUsd"
{-# DEPRECATED pwtpAmountInUsd "Use generic-lens or generic-optics with 'amountInUsd' instead." #-}

instance Core.FromJSON PublicWorkforceTaskPrice where
  toJSON PublicWorkforceTaskPrice {..} =
    Core.object
      (Core.catMaybes [("AmountInUsd" Core..=) Core.<$> amountInUsd])

instance Core.FromJSON PublicWorkforceTaskPrice where
  parseJSON =
    Core.withObject "PublicWorkforceTaskPrice" Core.$
      \x ->
        PublicWorkforceTaskPrice' Core.<$> (x Core..:? "AmountInUsd")
