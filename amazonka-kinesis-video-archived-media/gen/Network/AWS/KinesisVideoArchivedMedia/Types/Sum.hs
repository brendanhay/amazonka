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

data ContainerFormat
  = FragmentedMP4
  | MpegTs
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText ContainerFormat where
    parser = takeLowerText >>= \case
        "fragmented_mp4" -> pure FragmentedMP4
        "mpeg_ts" -> pure MpegTs
        e -> fromTextError $ "Failure parsing ContainerFormat from value: '" <> e
           <> "'. Accepted values: fragmented_mp4, mpeg_ts"

instance ToText ContainerFormat where
    toText = \case
        FragmentedMP4 -> "FRAGMENTED_MP4"
        MpegTs -> "MPEG_TS"

instance Hashable     ContainerFormat
instance NFData       ContainerFormat
instance ToByteString ContainerFormat
instance ToQuery      ContainerFormat
instance ToHeader     ContainerFormat

instance ToJSON ContainerFormat where
    toJSON = toJSONText

data DiscontinuityMode
  = DMAlways
  | DMNever
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DiscontinuityMode where
    parser = takeLowerText >>= \case
        "always" -> pure DMAlways
        "never" -> pure DMNever
        e -> fromTextError $ "Failure parsing DiscontinuityMode from value: '" <> e
           <> "'. Accepted values: always, never"

instance ToText DiscontinuityMode where
    toText = \case
        DMAlways -> "ALWAYS"
        DMNever -> "NEVER"

instance Hashable     DiscontinuityMode
instance NFData       DiscontinuityMode
instance ToByteString DiscontinuityMode
instance ToQuery      DiscontinuityMode
instance ToHeader     DiscontinuityMode

instance ToJSON DiscontinuityMode where
    toJSON = toJSONText

data DisplayFragmentTimestamp
  = Always
  | Never
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText DisplayFragmentTimestamp where
    parser = takeLowerText >>= \case
        "always" -> pure Always
        "never" -> pure Never
        e -> fromTextError $ "Failure parsing DisplayFragmentTimestamp from value: '" <> e
           <> "'. Accepted values: always, never"

instance ToText DisplayFragmentTimestamp where
    toText = \case
        Always -> "ALWAYS"
        Never -> "NEVER"

instance Hashable     DisplayFragmentTimestamp
instance NFData       DisplayFragmentTimestamp
instance ToByteString DisplayFragmentTimestamp
instance ToQuery      DisplayFragmentTimestamp
instance ToHeader     DisplayFragmentTimestamp

instance ToJSON DisplayFragmentTimestamp where
    toJSON = toJSONText

data FragmentSelectorType
  = FSTProducerTimestamp
  | FSTServerTimestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText FragmentSelectorType where
    parser = takeLowerText >>= \case
        "producer_timestamp" -> pure FSTProducerTimestamp
        "server_timestamp" -> pure FSTServerTimestamp
        e -> fromTextError $ "Failure parsing FragmentSelectorType from value: '" <> e
           <> "'. Accepted values: producer_timestamp, server_timestamp"

instance ToText FragmentSelectorType where
    toText = \case
        FSTProducerTimestamp -> "PRODUCER_TIMESTAMP"
        FSTServerTimestamp -> "SERVER_TIMESTAMP"

instance Hashable     FragmentSelectorType
instance NFData       FragmentSelectorType
instance ToByteString FragmentSelectorType
instance ToQuery      FragmentSelectorType
instance ToHeader     FragmentSelectorType

instance ToJSON FragmentSelectorType where
    toJSON = toJSONText

data HLSFragmentSelectorType
  = ProducerTimestamp
  | ServerTimestamp
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText HLSFragmentSelectorType where
    parser = takeLowerText >>= \case
        "producer_timestamp" -> pure ProducerTimestamp
        "server_timestamp" -> pure ServerTimestamp
        e -> fromTextError $ "Failure parsing HLSFragmentSelectorType from value: '" <> e
           <> "'. Accepted values: producer_timestamp, server_timestamp"

instance ToText HLSFragmentSelectorType where
    toText = \case
        ProducerTimestamp -> "PRODUCER_TIMESTAMP"
        ServerTimestamp -> "SERVER_TIMESTAMP"

instance Hashable     HLSFragmentSelectorType
instance NFData       HLSFragmentSelectorType
instance ToByteString HLSFragmentSelectorType
instance ToQuery      HLSFragmentSelectorType
instance ToHeader     HLSFragmentSelectorType

instance ToJSON HLSFragmentSelectorType where
    toJSON = toJSONText

data PlaybackMode
  = Live
  | OnDemand
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlaybackMode where
    parser = takeLowerText >>= \case
        "live" -> pure Live
        "on_demand" -> pure OnDemand
        e -> fromTextError $ "Failure parsing PlaybackMode from value: '" <> e
           <> "'. Accepted values: live, on_demand"

instance ToText PlaybackMode where
    toText = \case
        Live -> "LIVE"
        OnDemand -> "ON_DEMAND"

instance Hashable     PlaybackMode
instance NFData       PlaybackMode
instance ToByteString PlaybackMode
instance ToQuery      PlaybackMode
instance ToHeader     PlaybackMode

instance ToJSON PlaybackMode where
    toJSON = toJSONText
