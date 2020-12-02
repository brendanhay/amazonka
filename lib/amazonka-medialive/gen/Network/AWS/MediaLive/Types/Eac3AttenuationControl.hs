{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Eac3AttenuationControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Eac3AttenuationControl where

import Network.AWS.Prelude

-- | Eac3 Attenuation Control
data Eac3AttenuationControl
  = EACAttenuate3DB
  | EACNone
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

instance FromText Eac3AttenuationControl where
  parser =
    takeLowerText >>= \case
      "attenuate_3_db" -> pure EACAttenuate3DB
      "none" -> pure EACNone
      e ->
        fromTextError $
          "Failure parsing Eac3AttenuationControl from value: '" <> e
            <> "'. Accepted values: attenuate_3_db, none"

instance ToText Eac3AttenuationControl where
  toText = \case
    EACAttenuate3DB -> "ATTENUATE_3_DB"
    EACNone -> "NONE"

instance Hashable Eac3AttenuationControl

instance NFData Eac3AttenuationControl

instance ToByteString Eac3AttenuationControl

instance ToQuery Eac3AttenuationControl

instance ToHeader Eac3AttenuationControl

instance ToJSON Eac3AttenuationControl where
  toJSON = toJSONText

instance FromJSON Eac3AttenuationControl where
  parseJSON = parseJSONText "Eac3AttenuationControl"
