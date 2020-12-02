{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.BillingDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.BillingDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that describes workflow billing details.
--
--
--
-- /See:/ 'billingDetails' smart constructor.
data BillingDetails = BillingDetails'
  { _bdBilledMemoryUsedInMB ::
      !(Maybe Nat),
    _bdBilledDurationInMilliseconds :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BillingDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdBilledMemoryUsedInMB' - Billed memory consumption of your workflow, in MB.
--
-- * 'bdBilledDurationInMilliseconds' - Billed duration of your workflow, in milliseconds.
billingDetails ::
  BillingDetails
billingDetails =
  BillingDetails'
    { _bdBilledMemoryUsedInMB = Nothing,
      _bdBilledDurationInMilliseconds = Nothing
    }

-- | Billed memory consumption of your workflow, in MB.
bdBilledMemoryUsedInMB :: Lens' BillingDetails (Maybe Natural)
bdBilledMemoryUsedInMB = lens _bdBilledMemoryUsedInMB (\s a -> s {_bdBilledMemoryUsedInMB = a}) . mapping _Nat

-- | Billed duration of your workflow, in milliseconds.
bdBilledDurationInMilliseconds :: Lens' BillingDetails (Maybe Natural)
bdBilledDurationInMilliseconds = lens _bdBilledDurationInMilliseconds (\s a -> s {_bdBilledDurationInMilliseconds = a}) . mapping _Nat

instance FromJSON BillingDetails where
  parseJSON =
    withObject
      "BillingDetails"
      ( \x ->
          BillingDetails'
            <$> (x .:? "billedMemoryUsedInMB")
            <*> (x .:? "billedDurationInMilliseconds")
      )

instance Hashable BillingDetails

instance NFData BillingDetails
