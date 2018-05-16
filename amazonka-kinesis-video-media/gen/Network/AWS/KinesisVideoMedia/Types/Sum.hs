{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoMedia.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoMedia.Types.Sum where

import Network.AWS.Prelude

data StartSelectorType
  = ContinuationToken
  | Earliest
  | FragmentNumber
  | Now
  | ProducerTimestamp
  | ServerTimestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StartSelectorType where
    parser = takeLowerText >>= \case
        "continuation_token" -> pure ContinuationToken
        "earliest" -> pure Earliest
        "fragment_number" -> pure FragmentNumber
        "now" -> pure Now
        "producer_timestamp" -> pure ProducerTimestamp
        "server_timestamp" -> pure ServerTimestamp
        e -> fromTextError $ "Failure parsing StartSelectorType from value: '" <> e
           <> "'. Accepted values: continuation_token, earliest, fragment_number, now, producer_timestamp, server_timestamp"

instance ToText StartSelectorType where
    toText = \case
        ContinuationToken -> "CONTINUATION_TOKEN"
        Earliest -> "EARLIEST"
        FragmentNumber -> "FRAGMENT_NUMBER"
        Now -> "NOW"
        ProducerTimestamp -> "PRODUCER_TIMESTAMP"
        ServerTimestamp -> "SERVER_TIMESTAMP"

instance Hashable     StartSelectorType
instance NFData       StartSelectorType
instance ToByteString StartSelectorType
instance ToQuery      StartSelectorType
instance ToHeader     StartSelectorType

instance ToJSON StartSelectorType where
    toJSON = toJSONText
