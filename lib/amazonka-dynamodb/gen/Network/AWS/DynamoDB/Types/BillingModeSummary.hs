{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.BillingModeSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.BillingModeSummary where

import Network.AWS.DynamoDB.Types.BillingMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains the details for the read/write capacity mode.
--
--
--
-- /See:/ 'billingModeSummary' smart constructor.
data BillingModeSummary = BillingModeSummary'
  { _bmsLastUpdateToPayPerRequestDateTime ::
      !(Maybe POSIX),
    _bmsBillingMode :: !(Maybe BillingMode)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BillingModeSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmsLastUpdateToPayPerRequestDateTime' - Represents the time when @PAY_PER_REQUEST@ was last set as the read/write capacity mode.
--
-- * 'bmsBillingMode' - Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
billingModeSummary ::
  BillingModeSummary
billingModeSummary =
  BillingModeSummary'
    { _bmsLastUpdateToPayPerRequestDateTime =
        Nothing,
      _bmsBillingMode = Nothing
    }

-- | Represents the time when @PAY_PER_REQUEST@ was last set as the read/write capacity mode.
bmsLastUpdateToPayPerRequestDateTime :: Lens' BillingModeSummary (Maybe UTCTime)
bmsLastUpdateToPayPerRequestDateTime = lens _bmsLastUpdateToPayPerRequestDateTime (\s a -> s {_bmsLastUpdateToPayPerRequestDateTime = a}) . mapping _Time

-- | Controls how you are charged for read and write throughput and how you manage capacity. This setting can be changed later.     * @PROVISIONED@ - Sets the read/write capacity mode to @PROVISIONED@ . We recommend using @PROVISIONED@ for predictable workloads.     * @PAY_PER_REQUEST@ - Sets the read/write capacity mode to @PAY_PER_REQUEST@ . We recommend using @PAY_PER_REQUEST@ for unpredictable workloads.
bmsBillingMode :: Lens' BillingModeSummary (Maybe BillingMode)
bmsBillingMode = lens _bmsBillingMode (\s a -> s {_bmsBillingMode = a})

instance FromJSON BillingModeSummary where
  parseJSON =
    withObject
      "BillingModeSummary"
      ( \x ->
          BillingModeSummary'
            <$> (x .:? "LastUpdateToPayPerRequestDateTime")
            <*> (x .:? "BillingMode")
      )

instance Hashable BillingModeSummary

instance NFData BillingModeSummary
