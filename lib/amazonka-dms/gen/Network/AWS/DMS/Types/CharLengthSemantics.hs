{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.CharLengthSemantics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.CharLengthSemantics where

import Network.AWS.Prelude

data CharLengthSemantics
  = Byte
  | Char
  | Default
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

instance FromText CharLengthSemantics where
  parser =
    takeLowerText >>= \case
      "byte" -> pure Byte
      "char" -> pure Char
      "default" -> pure Default
      e ->
        fromTextError $
          "Failure parsing CharLengthSemantics from value: '" <> e
            <> "'. Accepted values: byte, char, default"

instance ToText CharLengthSemantics where
  toText = \case
    Byte -> "byte"
    Char -> "char"
    Default -> "default"

instance Hashable CharLengthSemantics

instance NFData CharLengthSemantics

instance ToByteString CharLengthSemantics

instance ToQuery CharLengthSemantics

instance ToHeader CharLengthSemantics

instance ToJSON CharLengthSemantics where
  toJSON = toJSONText

instance FromJSON CharLengthSemantics where
  parseJSON = parseJSONText "CharLengthSemantics"
