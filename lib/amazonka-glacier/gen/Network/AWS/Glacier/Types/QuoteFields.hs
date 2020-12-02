{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.Types.QuoteFields
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.QuoteFields where

import Network.AWS.Prelude

data QuoteFields
  = ASNeeded
  | Always
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

instance FromText QuoteFields where
  parser =
    takeLowerText >>= \case
      "asneeded" -> pure ASNeeded
      "always" -> pure Always
      e ->
        fromTextError $
          "Failure parsing QuoteFields from value: '" <> e
            <> "'. Accepted values: asneeded, always"

instance ToText QuoteFields where
  toText = \case
    ASNeeded -> "ASNEEDED"
    Always -> "ALWAYS"

instance Hashable QuoteFields

instance NFData QuoteFields

instance ToByteString QuoteFields

instance ToQuery QuoteFields

instance ToHeader QuoteFields

instance ToJSON QuoteFields where
  toJSON = toJSONText

instance FromJSON QuoteFields where
  parseJSON = parseJSONText "QuoteFields"
