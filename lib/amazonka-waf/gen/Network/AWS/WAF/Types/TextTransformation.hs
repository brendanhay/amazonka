{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.TextTransformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.TextTransformation where

import Network.AWS.Prelude

data TextTransformation
  = CmdLine
  | CompressWhiteSpace
  | HTMLEntityDecode
  | Lowercase
  | None
  | URLDecode
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

instance FromText TextTransformation where
  parser =
    takeLowerText >>= \case
      "cmd_line" -> pure CmdLine
      "compress_white_space" -> pure CompressWhiteSpace
      "html_entity_decode" -> pure HTMLEntityDecode
      "lowercase" -> pure Lowercase
      "none" -> pure None
      "url_decode" -> pure URLDecode
      e ->
        fromTextError $
          "Failure parsing TextTransformation from value: '" <> e
            <> "'. Accepted values: cmd_line, compress_white_space, html_entity_decode, lowercase, none, url_decode"

instance ToText TextTransformation where
  toText = \case
    CmdLine -> "CMD_LINE"
    CompressWhiteSpace -> "COMPRESS_WHITE_SPACE"
    HTMLEntityDecode -> "HTML_ENTITY_DECODE"
    Lowercase -> "LOWERCASE"
    None -> "NONE"
    URLDecode -> "URL_DECODE"

instance Hashable TextTransformation

instance NFData TextTransformation

instance ToByteString TextTransformation

instance ToQuery TextTransformation

instance ToHeader TextTransformation

instance ToJSON TextTransformation where
  toJSON = toJSONText

instance FromJSON TextTransformation where
  parseJSON = parseJSONText "TextTransformation"
