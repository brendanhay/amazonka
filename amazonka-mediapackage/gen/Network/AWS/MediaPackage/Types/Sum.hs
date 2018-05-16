{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.Sum
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaPackage.Types.Sum where

import Network.AWS.Prelude

data AdMarkers
  = AMNone
  | AMPassthrough
  | AMSCTE35Enhanced
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText AdMarkers where
    parser = takeLowerText >>= \case
        "none" -> pure AMNone
        "passthrough" -> pure AMPassthrough
        "scte35_enhanced" -> pure AMSCTE35Enhanced
        e -> fromTextError $ "Failure parsing AdMarkers from value: '" <> e
           <> "'. Accepted values: none, passthrough, scte35_enhanced"

instance ToText AdMarkers where
    toText = \case
        AMNone -> "NONE"
        AMPassthrough -> "PASSTHROUGH"
        AMSCTE35Enhanced -> "SCTE35_ENHANCED"

instance Hashable     AdMarkers
instance NFData       AdMarkers
instance ToByteString AdMarkers
instance ToQuery      AdMarkers
instance ToHeader     AdMarkers

instance ToJSON AdMarkers where
    toJSON = toJSONText

instance FromJSON AdMarkers where
    parseJSON = parseJSONText "AdMarkers"

data EncryptionMethod
  = AES128
  | SampleAES
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText EncryptionMethod where
    parser = takeLowerText >>= \case
        "aes_128" -> pure AES128
        "sample_aes" -> pure SampleAES
        e -> fromTextError $ "Failure parsing EncryptionMethod from value: '" <> e
           <> "'. Accepted values: aes_128, sample_aes"

instance ToText EncryptionMethod where
    toText = \case
        AES128 -> "AES_128"
        SampleAES -> "SAMPLE_AES"

instance Hashable     EncryptionMethod
instance NFData       EncryptionMethod
instance ToByteString EncryptionMethod
instance ToQuery      EncryptionMethod
instance ToHeader     EncryptionMethod

instance ToJSON EncryptionMethod where
    toJSON = toJSONText

instance FromJSON EncryptionMethod where
    parseJSON = parseJSONText "EncryptionMethod"

data PlaylistType
  = Event
  | None
  | Vod
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText PlaylistType where
    parser = takeLowerText >>= \case
        "event" -> pure Event
        "none" -> pure None
        "vod" -> pure Vod
        e -> fromTextError $ "Failure parsing PlaylistType from value: '" <> e
           <> "'. Accepted values: event, none, vod"

instance ToText PlaylistType where
    toText = \case
        Event -> "EVENT"
        None -> "NONE"
        Vod -> "VOD"

instance Hashable     PlaylistType
instance NFData       PlaylistType
instance ToByteString PlaylistType
instance ToQuery      PlaylistType
instance ToHeader     PlaylistType

instance ToJSON PlaylistType where
    toJSON = toJSONText

instance FromJSON PlaylistType where
    parseJSON = parseJSONText "PlaylistType"

data Profile
  = PHbbtv15
  | PNone
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText Profile where
    parser = takeLowerText >>= \case
        "hbbtv_1_5" -> pure PHbbtv15
        "none" -> pure PNone
        e -> fromTextError $ "Failure parsing Profile from value: '" <> e
           <> "'. Accepted values: hbbtv_1_5, none"

instance ToText Profile where
    toText = \case
        PHbbtv15 -> "HBBTV_1_5"
        PNone -> "NONE"

instance Hashable     Profile
instance NFData       Profile
instance ToByteString Profile
instance ToQuery      Profile
instance ToHeader     Profile

instance ToJSON Profile where
    toJSON = toJSONText

instance FromJSON Profile where
    parseJSON = parseJSONText "Profile"

data StreamOrder
  = Original
  | VideoBitrateAscending
  | VideoBitrateDescending
  deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)


instance FromText StreamOrder where
    parser = takeLowerText >>= \case
        "original" -> pure Original
        "video_bitrate_ascending" -> pure VideoBitrateAscending
        "video_bitrate_descending" -> pure VideoBitrateDescending
        e -> fromTextError $ "Failure parsing StreamOrder from value: '" <> e
           <> "'. Accepted values: original, video_bitrate_ascending, video_bitrate_descending"

instance ToText StreamOrder where
    toText = \case
        Original -> "ORIGINAL"
        VideoBitrateAscending -> "VIDEO_BITRATE_ASCENDING"
        VideoBitrateDescending -> "VIDEO_BITRATE_DESCENDING"

instance Hashable     StreamOrder
instance NFData       StreamOrder
instance ToByteString StreamOrder
instance ToQuery      StreamOrder
instance ToHeader     StreamOrder

instance ToJSON StreamOrder where
    toJSON = toJSONText

instance FromJSON StreamOrder where
    parseJSON = parseJSONText "StreamOrder"
