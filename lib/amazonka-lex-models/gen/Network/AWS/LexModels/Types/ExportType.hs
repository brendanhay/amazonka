{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ExportType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ExportType where

import Network.AWS.Prelude

data ExportType
  = AlexaSkillsKit
  | Lex
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

instance FromText ExportType where
  parser =
    takeLowerText >>= \case
      "alexa_skills_kit" -> pure AlexaSkillsKit
      "lex" -> pure Lex
      e ->
        fromTextError $
          "Failure parsing ExportType from value: '" <> e
            <> "'. Accepted values: alexa_skills_kit, lex"

instance ToText ExportType where
  toText = \case
    AlexaSkillsKit -> "ALEXA_SKILLS_KIT"
    Lex -> "LEX"

instance Hashable ExportType

instance NFData ExportType

instance ToByteString ExportType

instance ToQuery ExportType

instance ToHeader ExportType

instance ToJSON ExportType where
  toJSON = toJSONText

instance FromJSON ExportType where
  parseJSON = parseJSONText "ExportType"
