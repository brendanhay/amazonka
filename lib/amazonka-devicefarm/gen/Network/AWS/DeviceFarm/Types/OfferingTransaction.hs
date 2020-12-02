{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.OfferingTransaction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.OfferingTransaction where

import Network.AWS.DeviceFarm.Types.MonetaryAmount
import Network.AWS.DeviceFarm.Types.OfferingStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the metadata of an offering transaction.
--
--
--
-- /See:/ 'offeringTransaction' smart constructor.
data OfferingTransaction = OfferingTransaction'
  { _otOfferingStatus ::
      !(Maybe OfferingStatus),
    _otCost :: !(Maybe MonetaryAmount),
    _otTransactionId :: !(Maybe Text),
    _otOfferingPromotionId :: !(Maybe Text),
    _otCreatedOn :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OfferingTransaction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'otOfferingStatus' - The status of an offering transaction.
--
-- * 'otCost' - The cost of an offering transaction.
--
-- * 'otTransactionId' - The transaction ID of the offering transaction.
--
-- * 'otOfferingPromotionId' - The ID that corresponds to a device offering promotion.
--
-- * 'otCreatedOn' - The date on which an offering transaction was created.
offeringTransaction ::
  OfferingTransaction
offeringTransaction =
  OfferingTransaction'
    { _otOfferingStatus = Nothing,
      _otCost = Nothing,
      _otTransactionId = Nothing,
      _otOfferingPromotionId = Nothing,
      _otCreatedOn = Nothing
    }

-- | The status of an offering transaction.
otOfferingStatus :: Lens' OfferingTransaction (Maybe OfferingStatus)
otOfferingStatus = lens _otOfferingStatus (\s a -> s {_otOfferingStatus = a})

-- | The cost of an offering transaction.
otCost :: Lens' OfferingTransaction (Maybe MonetaryAmount)
otCost = lens _otCost (\s a -> s {_otCost = a})

-- | The transaction ID of the offering transaction.
otTransactionId :: Lens' OfferingTransaction (Maybe Text)
otTransactionId = lens _otTransactionId (\s a -> s {_otTransactionId = a})

-- | The ID that corresponds to a device offering promotion.
otOfferingPromotionId :: Lens' OfferingTransaction (Maybe Text)
otOfferingPromotionId = lens _otOfferingPromotionId (\s a -> s {_otOfferingPromotionId = a})

-- | The date on which an offering transaction was created.
otCreatedOn :: Lens' OfferingTransaction (Maybe UTCTime)
otCreatedOn = lens _otCreatedOn (\s a -> s {_otCreatedOn = a}) . mapping _Time

instance FromJSON OfferingTransaction where
  parseJSON =
    withObject
      "OfferingTransaction"
      ( \x ->
          OfferingTransaction'
            <$> (x .:? "offeringStatus")
            <*> (x .:? "cost")
            <*> (x .:? "transactionId")
            <*> (x .:? "offeringPromotionId")
            <*> (x .:? "createdOn")
      )

instance Hashable OfferingTransaction

instance NFData OfferingTransaction
