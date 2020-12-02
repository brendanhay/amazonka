{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.USD

-- | Defines the amount of money paid to an Amazon Mechanical Turk worker for each task performed.
--
--
-- Use one of the following prices for bounding box tasks. Prices are in US dollars and should be based on the complexity of the task; the longer it takes in your initial testing, the more you should offer.
--
--     * 0.036
--
--     * 0.048
--
--     * 0.060
--
--     * 0.072
--
--     * 0.120
--
--     * 0.240
--
--     * 0.360
--
--     * 0.480
--
--     * 0.600
--
--     * 0.720
--
--     * 0.840
--
--     * 0.960
--
--     * 1.080
--
--     * 1.200
--
--
--
-- Use one of the following prices for image classification, text classification, and custom tasks. Prices are in US dollars.
--
--     * 0.012
--
--     * 0.024
--
--     * 0.036
--
--     * 0.048
--
--     * 0.060
--
--     * 0.072
--
--     * 0.120
--
--     * 0.240
--
--     * 0.360
--
--     * 0.480
--
--     * 0.600
--
--     * 0.720
--
--     * 0.840
--
--     * 0.960
--
--     * 1.080
--
--     * 1.200
--
--
--
-- Use one of the following prices for semantic segmentation tasks. Prices are in US dollars.
--
--     * 0.840
--
--     * 0.960
--
--     * 1.080
--
--     * 1.200
--
--
--
-- Use one of the following prices for Textract AnalyzeDocument Important Form Key Amazon Augmented AI review tasks. Prices are in US dollars.
--
--     * 2.400
--
--     * 2.280
--
--     * 2.160
--
--     * 2.040
--
--     * 1.920
--
--     * 1.800
--
--     * 1.680
--
--     * 1.560
--
--     * 1.440
--
--     * 1.320
--
--     * 1.200
--
--     * 1.080
--
--     * 0.960
--
--     * 0.840
--
--     * 0.720
--
--     * 0.600
--
--     * 0.480
--
--     * 0.360
--
--     * 0.240
--
--     * 0.120
--
--     * 0.072
--
--     * 0.060
--
--     * 0.048
--
--     * 0.036
--
--     * 0.024
--
--     * 0.012
--
--
--
-- Use one of the following prices for Rekognition DetectModerationLabels Amazon Augmented AI review tasks. Prices are in US dollars.
--
--     * 1.200
--
--     * 1.080
--
--     * 0.960
--
--     * 0.840
--
--     * 0.720
--
--     * 0.600
--
--     * 0.480
--
--     * 0.360
--
--     * 0.240
--
--     * 0.120
--
--     * 0.072
--
--     * 0.060
--
--     * 0.048
--
--     * 0.036
--
--     * 0.024
--
--     * 0.012
--
--
--
-- Use one of the following prices for Amazon Augmented AI custom human review tasks. Prices are in US dollars.
--
--     * 1.200
--
--     * 1.080
--
--     * 0.960
--
--     * 0.840
--
--     * 0.720
--
--     * 0.600
--
--     * 0.480
--
--     * 0.360
--
--     * 0.240
--
--     * 0.120
--
--     * 0.072
--
--     * 0.060
--
--     * 0.048
--
--     * 0.036
--
--     * 0.024
--
--     * 0.012
--
--
--
--
-- /See:/ 'publicWorkforceTaskPrice' smart constructor.
newtype PublicWorkforceTaskPrice = PublicWorkforceTaskPrice'
  { _pwtpAmountInUsd ::
      Maybe USD
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PublicWorkforceTaskPrice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pwtpAmountInUsd' - Defines the amount of money paid to an Amazon Mechanical Turk worker in United States dollars.
publicWorkforceTaskPrice ::
  PublicWorkforceTaskPrice
publicWorkforceTaskPrice =
  PublicWorkforceTaskPrice' {_pwtpAmountInUsd = Nothing}

-- | Defines the amount of money paid to an Amazon Mechanical Turk worker in United States dollars.
pwtpAmountInUsd :: Lens' PublicWorkforceTaskPrice (Maybe USD)
pwtpAmountInUsd = lens _pwtpAmountInUsd (\s a -> s {_pwtpAmountInUsd = a})

instance FromJSON PublicWorkforceTaskPrice where
  parseJSON =
    withObject
      "PublicWorkforceTaskPrice"
      (\x -> PublicWorkforceTaskPrice' <$> (x .:? "AmountInUsd"))

instance Hashable PublicWorkforceTaskPrice

instance NFData PublicWorkforceTaskPrice

instance ToJSON PublicWorkforceTaskPrice where
  toJSON PublicWorkforceTaskPrice' {..} =
    object (catMaybes [("AmountInUsd" .=) <$> _pwtpAmountInUsd])
