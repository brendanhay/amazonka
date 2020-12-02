{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateType where

import Network.AWS.Prelude

data TemplateType
  = TTEmail
  | TTPush
  | TTSms
  | TTVoice
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

instance FromText TemplateType where
  parser =
    takeLowerText >>= \case
      "email" -> pure TTEmail
      "push" -> pure TTPush
      "sms" -> pure TTSms
      "voice" -> pure TTVoice
      e ->
        fromTextError $
          "Failure parsing TemplateType from value: '" <> e
            <> "'. Accepted values: email, push, sms, voice"

instance ToText TemplateType where
  toText = \case
    TTEmail -> "EMAIL"
    TTPush -> "PUSH"
    TTSms -> "SMS"
    TTVoice -> "VOICE"

instance Hashable TemplateType

instance NFData TemplateType

instance ToByteString TemplateType

instance ToQuery TemplateType

instance ToHeader TemplateType

instance FromJSON TemplateType where
  parseJSON = parseJSONText "TemplateType"
