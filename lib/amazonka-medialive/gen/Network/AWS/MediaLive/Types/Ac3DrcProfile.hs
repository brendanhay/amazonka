{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Ac3DrcProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Ac3DrcProfile where

import Network.AWS.Prelude

-- | Ac3 Drc Profile
data Ac3DrcProfile
  = ADPFilmStandard
  | ADPNone
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

instance FromText Ac3DrcProfile where
  parser =
    takeLowerText >>= \case
      "film_standard" -> pure ADPFilmStandard
      "none" -> pure ADPNone
      e ->
        fromTextError $
          "Failure parsing Ac3DrcProfile from value: '" <> e
            <> "'. Accepted values: film_standard, none"

instance ToText Ac3DrcProfile where
  toText = \case
    ADPFilmStandard -> "FILM_STANDARD"
    ADPNone -> "NONE"

instance Hashable Ac3DrcProfile

instance NFData Ac3DrcProfile

instance ToByteString Ac3DrcProfile

instance ToQuery Ac3DrcProfile

instance ToHeader Ac3DrcProfile

instance ToJSON Ac3DrcProfile where
  toJSON = toJSONText

instance FromJSON Ac3DrcProfile where
  parseJSON = parseJSONText "Ac3DrcProfile"
