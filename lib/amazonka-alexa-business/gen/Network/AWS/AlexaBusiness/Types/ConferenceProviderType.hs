{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.ConferenceProviderType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.ConferenceProviderType where

import Network.AWS.Prelude

data ConferenceProviderType
  = Bluejeans
  | Chime
  | Custom
  | Fuze
  | GoogleHangouts
  | Polycom
  | Ringcentral
  | SkypeForBusiness
  | Webex
  | Zoom
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText ConferenceProviderType where
  parser =
    takeLowerText >>= \case
      "bluejeans" -> pure Bluejeans
      "chime" -> pure Chime
      "custom" -> pure Custom
      "fuze" -> pure Fuze
      "google_hangouts" -> pure GoogleHangouts
      "polycom" -> pure Polycom
      "ringcentral" -> pure Ringcentral
      "skype_for_business" -> pure SkypeForBusiness
      "webex" -> pure Webex
      "zoom" -> pure Zoom
      e ->
        fromTextError $
          "Failure parsing ConferenceProviderType from value: '" <> e
            <> "'. Accepted values: bluejeans, chime, custom, fuze, google_hangouts, polycom, ringcentral, skype_for_business, webex, zoom"

instance ToText ConferenceProviderType where
  toText = \case
    Bluejeans -> "BLUEJEANS"
    Chime -> "CHIME"
    Custom -> "CUSTOM"
    Fuze -> "FUZE"
    GoogleHangouts -> "GOOGLE_HANGOUTS"
    Polycom -> "POLYCOM"
    Ringcentral -> "RINGCENTRAL"
    SkypeForBusiness -> "SKYPE_FOR_BUSINESS"
    Webex -> "WEBEX"
    Zoom -> "ZOOM"

instance Hashable ConferenceProviderType

instance NFData ConferenceProviderType

instance ToByteString ConferenceProviderType

instance ToQuery ConferenceProviderType

instance ToHeader ConferenceProviderType

instance ToJSON ConferenceProviderType where
  toJSON = toJSONText

instance FromJSON ConferenceProviderType where
  parseJSON = parseJSONText "ConferenceProviderType"
