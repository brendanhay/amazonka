{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MarketplaceMetering.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MarketplaceMetering.Types.Sum where

import Network.AWS.Prelude

data UsageRecordResultStatus
  = CustomerNotSubscribed
  | DuplicateRecord
  | Success
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText UsageRecordResultStatus where
    parser = takeLowerText >>= \case
        "customernotsubscribed" -> pure CustomerNotSubscribed
        "duplicaterecord" -> pure DuplicateRecord
        "success" -> pure Success
        e -> fromTextError $ "Failure parsing UsageRecordResultStatus from value: '" <> e
           <> "'. Accepted values: customernotsubscribed, duplicaterecord, success"

instance ToText UsageRecordResultStatus where
    toText = \case
        CustomerNotSubscribed -> "CustomerNotSubscribed"
        DuplicateRecord -> "DuplicateRecord"
        Success -> "Success"

instance Hashable     UsageRecordResultStatus
instance NFData       UsageRecordResultStatus
instance ToByteString UsageRecordResultStatus
instance ToQuery      UsageRecordResultStatus
instance ToHeader     UsageRecordResultStatus

instance FromJSON UsageRecordResultStatus where
    parseJSON = parseJSONText "UsageRecordResultStatus"
