{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.LoaContentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.LoaContentType where

import Network.AWS.Prelude

data LoaContentType = ApplicationPdf
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

instance FromText LoaContentType where
  parser =
    takeLowerText >>= \case
      "application/pdf" -> pure ApplicationPdf
      e ->
        fromTextError $
          "Failure parsing LoaContentType from value: '" <> e
            <> "'. Accepted values: application/pdf"

instance ToText LoaContentType where
  toText = \case
    ApplicationPdf -> "application/pdf"

instance Hashable LoaContentType

instance NFData LoaContentType

instance ToByteString LoaContentType

instance ToQuery LoaContentType

instance ToHeader LoaContentType

instance ToJSON LoaContentType where
  toJSON = toJSONText

instance FromJSON LoaContentType where
  parseJSON = parseJSONText "LoaContentType"
