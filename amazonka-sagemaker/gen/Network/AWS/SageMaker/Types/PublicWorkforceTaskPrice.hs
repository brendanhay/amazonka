{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.USD

-- | Defines the amount of money paid to an Amazon Mechanical Turk worker for
-- each task performed.
--
-- Use one of the following prices for bounding box tasks. Prices are in US
-- dollars and should be based on the complexity of the task; the longer it
-- takes in your initial testing, the more you should offer.
--
-- -   0.036
--
-- -   0.048
--
-- -   0.060
--
-- -   0.072
--
-- -   0.120
--
-- -   0.240
--
-- -   0.360
--
-- -   0.480
--
-- -   0.600
--
-- -   0.720
--
-- -   0.840
--
-- -   0.960
--
-- -   1.080
--
-- -   1.200
--
-- Use one of the following prices for image classification, text
-- classification, and custom tasks. Prices are in US dollars.
--
-- -   0.012
--
-- -   0.024
--
-- -   0.036
--
-- -   0.048
--
-- -   0.060
--
-- -   0.072
--
-- -   0.120
--
-- -   0.240
--
-- -   0.360
--
-- -   0.480
--
-- -   0.600
--
-- -   0.720
--
-- -   0.840
--
-- -   0.960
--
-- -   1.080
--
-- -   1.200
--
-- Use one of the following prices for semantic segmentation tasks. Prices
-- are in US dollars.
--
-- -   0.840
--
-- -   0.960
--
-- -   1.080
--
-- -   1.200
--
-- Use one of the following prices for Textract AnalyzeDocument Important
-- Form Key Amazon Augmented AI review tasks. Prices are in US dollars.
--
-- -   2.400
--
-- -   2.280
--
-- -   2.160
--
-- -   2.040
--
-- -   1.920
--
-- -   1.800
--
-- -   1.680
--
-- -   1.560
--
-- -   1.440
--
-- -   1.320
--
-- -   1.200
--
-- -   1.080
--
-- -   0.960
--
-- -   0.840
--
-- -   0.720
--
-- -   0.600
--
-- -   0.480
--
-- -   0.360
--
-- -   0.240
--
-- -   0.120
--
-- -   0.072
--
-- -   0.060
--
-- -   0.048
--
-- -   0.036
--
-- -   0.024
--
-- -   0.012
--
-- Use one of the following prices for Rekognition DetectModerationLabels
-- Amazon Augmented AI review tasks. Prices are in US dollars.
--
-- -   1.200
--
-- -   1.080
--
-- -   0.960
--
-- -   0.840
--
-- -   0.720
--
-- -   0.600
--
-- -   0.480
--
-- -   0.360
--
-- -   0.240
--
-- -   0.120
--
-- -   0.072
--
-- -   0.060
--
-- -   0.048
--
-- -   0.036
--
-- -   0.024
--
-- -   0.012
--
-- Use one of the following prices for Amazon Augmented AI custom human
-- review tasks. Prices are in US dollars.
--
-- -   1.200
--
-- -   1.080
--
-- -   0.960
--
-- -   0.840
--
-- -   0.720
--
-- -   0.600
--
-- -   0.480
--
-- -   0.360
--
-- -   0.240
--
-- -   0.120
--
-- -   0.072
--
-- -   0.060
--
-- -   0.048
--
-- -   0.036
--
-- -   0.024
--
-- -   0.012
--
-- /See:/ 'newPublicWorkforceTaskPrice' smart constructor.
data PublicWorkforceTaskPrice = PublicWorkforceTaskPrice'
  { -- | Defines the amount of money paid to an Amazon Mechanical Turk worker in
    -- United States dollars.
    amountInUsd :: Prelude.Maybe USD
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PublicWorkforceTaskPrice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amountInUsd', 'publicWorkforceTaskPrice_amountInUsd' - Defines the amount of money paid to an Amazon Mechanical Turk worker in
-- United States dollars.
newPublicWorkforceTaskPrice ::
  PublicWorkforceTaskPrice
newPublicWorkforceTaskPrice =
  PublicWorkforceTaskPrice'
    { amountInUsd =
        Prelude.Nothing
    }

-- | Defines the amount of money paid to an Amazon Mechanical Turk worker in
-- United States dollars.
publicWorkforceTaskPrice_amountInUsd :: Lens.Lens' PublicWorkforceTaskPrice (Prelude.Maybe USD)
publicWorkforceTaskPrice_amountInUsd = Lens.lens (\PublicWorkforceTaskPrice' {amountInUsd} -> amountInUsd) (\s@PublicWorkforceTaskPrice' {} a -> s {amountInUsd = a} :: PublicWorkforceTaskPrice)

instance Prelude.FromJSON PublicWorkforceTaskPrice where
  parseJSON =
    Prelude.withObject
      "PublicWorkforceTaskPrice"
      ( \x ->
          PublicWorkforceTaskPrice'
            Prelude.<$> (x Prelude..:? "AmountInUsd")
      )

instance Prelude.Hashable PublicWorkforceTaskPrice

instance Prelude.NFData PublicWorkforceTaskPrice

instance Prelude.ToJSON PublicWorkforceTaskPrice where
  toJSON PublicWorkforceTaskPrice' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("AmountInUsd" Prelude..=) Prelude.<$> amountInUsd]
      )
