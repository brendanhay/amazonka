{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.ContentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.ContentType where

import Network.AWS.Prelude

data ContentType = ApplicationVnd_Amazonaws_Card_Generic
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

instance FromText ContentType where
  parser =
    takeLowerText >>= \case
      "application/vnd.amazonaws.card.generic" -> pure ApplicationVnd_Amazonaws_Card_Generic
      e ->
        fromTextError $
          "Failure parsing ContentType from value: '" <> e
            <> "'. Accepted values: application/vnd.amazonaws.card.generic"

instance ToText ContentType where
  toText = \case
    ApplicationVnd_Amazonaws_Card_Generic -> "application/vnd.amazonaws.card.generic"

instance Hashable ContentType

instance NFData ContentType

instance ToByteString ContentType

instance ToQuery ContentType

instance ToHeader ContentType

instance FromJSON ContentType where
  parseJSON = parseJSONText "ContentType"
