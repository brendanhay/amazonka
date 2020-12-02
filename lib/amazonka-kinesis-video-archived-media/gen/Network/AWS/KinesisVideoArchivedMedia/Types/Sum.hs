{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisVideoArchivedMedia.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisVideoArchivedMedia.Types.Sum where

import Network.AWS.Prelude

data FragmentSelectorType
  = ProducerTimestamp
  | ServerTimestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FragmentSelectorType where
    parser = takeLowerText >>= \case
        "producer_timestamp" -> pure ProducerTimestamp
        "server_timestamp" -> pure ServerTimestamp
        e -> fromTextError $ "Failure parsing FragmentSelectorType from value: '" <> e
           <> "'. Accepted values: producer_timestamp, server_timestamp"

instance ToText FragmentSelectorType where
    toText = \case
        ProducerTimestamp -> "PRODUCER_TIMESTAMP"
        ServerTimestamp -> "SERVER_TIMESTAMP"

instance Hashable     FragmentSelectorType
instance NFData       FragmentSelectorType
instance ToByteString FragmentSelectorType
instance ToQuery      FragmentSelectorType
instance ToHeader     FragmentSelectorType

instance ToJSON FragmentSelectorType where
    toJSON = toJSONText
